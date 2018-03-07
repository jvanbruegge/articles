# Inside the Cycle.js DOM driver

We often soley use an abstracted view of the DOM. Frameworks like React and Vue.js use virtual DOM while Glimmer uses a VM-like approach.

But how do you design an API that
- is easier to use?
- provides custom additional functionality?
- is efficient?

Until lately I did not know how exactly the Cycle.js DOM driver work, although I am a core team member of the project. I knew that it uses virtual DOM via a library called Snabbdom, but not much else.

In this article I want to guide you through my rewrite of the DOM driver, its motivations, how the algorithm works and which datastructures are delivering the performance you would expect from framework code.

# Motivations

In Cycle.js, we have concept called "isolation". A "component" is just a function, so it is just to group code together. Without isolation everything that happens in the component (like DOM events) will also happen in the parent.

```js
function Child(sources) {
    const clickCount$ = sources.DOM.events('click')
        .fold(acc => acc + 1, 0);

    return {
        DOM: clickCount$
        .map(count =>
            <div>
                <span>{ `In the child: ${count}` }</span>
                <button>Click me</button>
            </div>
        )
    };
}

function Parent(sources) {
    const child1 = Child(sources);
    const child2 = Child(sources);
    

    return {
        DOM: xs.combine(child1.DOM, child2.DOM)
        .map(arr => <div>{ arr }</div>)
    };
}
```
In this code, if we click on any button, both counters will increase. Thats because the events are not scoped by default. We can fix this with isolation:
```diff
function Child(sources) {
    const clickCount$ = sources.DOM.events('click')
        .fold(acc => acc + 1, 0);

    return {
        DOM: clickCount$
        .map(count =>
            <div>
                <span>{ `In the child: ${count}` }</span>
                <button>Click me</button>
            </div>
        )
    };
}

function Parent(sources) {
-    const child1 = Child(sources);
+    const child1 = isolate(Child, 'scope1')(sources);
-    const child2 = Child(sources);
+    const child2 = isolate(Child, 'scope2')(sources);
    

    return {
        DOM: xs.combine(child1.DOM, child2.DOM)
        .map(arr => <div>{ arr }</div>)
    };
}
```
With a simple call to the higher order function `isolate` we can apply scopes to our components. Those scopes have to be handled by the underlying drivers. So the DOM isolation will call functions in the DOM driver.

In the example above, we used total isolation. Any event that happens in the child will only be able to received in this child.

But later on we added a second isolation type: sibling isolation. This means the events are still isolated from the siblings of the component, but the parent can see them:
```js
function Child(sources) {
    const clickCount$ = sources.DOM.events('click')
        .fold(acc => acc + 1, 0);

    return {
        DOM: clickCount$
        .map(count =>
            <div>
                <span>{ `In the child: ${count}` }</span>
                <button>Click me</button>
            </div>
        )
    };
}

function Parent(sources) {
-    const child1 = isolate(Child, 'scope1')(sources);
+    const child1 = isolate(Child, '.scope1')(sources);
-    const child2 = isolate(Child, 'scope2')(sources);
+    const child2 = isolate(Child, '.scope2')(sources);
    
+    const parentCount$ = sources.DOM.events('click')
+        .fold(acc => acc + 1, 0);

    return {
-        DOM: xs.combine(child1.DOM, child2.DOM)
+        DOM: xs.combine(child1.DOM, child2.DOM, parentCount$)
            .map(arr => <div>{ arr }</div>)
    };
}
```
Now you will see a third counter, that will increase if any button is pressed, but the individual counters will still only increase if their button is pressed. If you try the same code with total isolation, the parent counter will stay at zero.

# The problem

Now this second isolation type was not integrated very well, because already the total isolation was basicly just bolted on the existing implementation. It was simply not designed to support multiple isolation types.

To fix those issues and to remove a lot of undefined behavior I decided to dive deep into the codebase and rewrite the whole isolation implementation and everything that is needed to support this new implementation.

# The overview

The most important question we have to ask is: How do we know which elements are in what isolation scope and how do we actually get this information?

For this you have to know that `isolate` basicly just calls `isolateSource` and `isolateSink` on the source object. You can imagine it like this:
```js
function isolate(component, scope) {
    return function isolatedComponent(sources) {
        const isolatedDOM = sources.DOM.isolateSource(sources.DOM, scope);
        const result = component({ ...sources, DOM: isolatedDOM });
        return {
            ...result,
            DOM: sources.DOM.isolateSink(result.DOM, scope)
        };
    };
}
```
As you can see we transform the source before passing it into the component and then we take the resulting stream and transform it again before returning it to the parent. Those two transformations are the only place where we can inject scope information. Furthermore, the source (sources.DOM) is an object, we can save stuff in there.

Using those ideas we can try adding scope information. We first define what a "scope" is. As we already saw, there are two types of isolation and they need a name to identify them (`scope1` in the earlier example). Let's just put them in an object:
```ts
interface Scope {
    type: 'total' | 'sibling';
    name: string;
}
```
Now, scopes can be nested (because you can nest your components), so just knowing one scope is not enough. We also have to know all scopes of the parent components. So we can use an array for that:
```ts
type Namespace = Array<Scope>;
```

So as we already saw we can only save stuff in our source object, so this would be the ideal place to store all parent scopes. We also saw that we can transform our source in `isolate`. Let's implement that:
```ts
class MainDOMSource {
    constructor(public readonly namespace: Namespace) {}

    public isolateSource(old: MainDOMSource, scope: string) {
         if(isSelector(scope)) {
             return new MainDOMSource(old.namespace.concat({ type: 'sibling', scope }));
        }
        return new MainDOMSource(old.namespace.concat({ type: 'total', scope }));
    }
}
```
Note the use of `concat`, it returns a **new** array without modifying the old one. This is important because the old one (aka the namespace of the parent) is still needed for itself and to construct other children.

So now we save the namespace of every component, but we are not using this knowledge. We have to somehow store the **elements** together with the namespace. But how do we actually get the DOM elements? Our components only return virtual dom elements.

# The isolate module

To solve this, our choice of virtual dom library becomes important. Snabbdom allows us to define so called modules. Those modules are objects with functions that hook in the DOM create/update/delete algorithm. Inside those hooks we can access the vnode and its newly created/updated/deleted DOM element (for more info you can see the [Snabbdom docs](https://github.com/snabbdom/snabbdom/blob/master/README.md#modules-documentation)).

But the hook only knows about the vnode, so we have to somehow get our scope information to the hook. Luckily this quite easy, we can use the second `isolate` transform (the one that transforms the vnode stream) to save the namespace stored in the source object into the vnode. Every vnode has a `data` object, where modules are free to add their own data. So let's add an `isolate` property on there:
```js
class MainDOMSource {
    constructor(public readonly namespace: Namespace) {}

    public isolateSource(old: MainDOMSource, scope: string) {
        return new MainDOMSource(old.namespace.concat(getScopeObject(scope)));
    }

    public isolateSink(old: Stream<VNode>, scope: string): Stream<VNode> {
        return old
       .map(vnode => ({
            ...vnode,
            data: {
                ...vnode.data,
                isolate: this.namespace.concat(getScopeObject(scope))
            }
       }));
}

function getScopeObject(scope: string): Scope {
    return {
        type: isSelector(scope) ? 'sibling' : 'total',
        scope
    };
}
```
