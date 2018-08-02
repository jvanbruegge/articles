# Inside a framework - How the Cycle.js DOM driver works

Often, we use a framework without really knowing how it works internally. Sometimes we contribute to that framework without having any clue about the inner workings.

For me, this was the case with Cycle.js. I was even invited to be a Core Team Member without having any clue how the DOM part of it worked besides "it uses virtual DOM under the hood".

Lately I stumbled across severe issues in the DOM driver that (together with older issues) convinced me to deep dive into it and rewrite it basicly from scratch.

In this article I want to show you the main algorithm and the data structures that make the DOM driver efficient, but still easy to use.

## The main problem - isolation

A Cycle.js component is just a pure function from some inputs (the sources) to some outputs (the sinks). This looks like this:
```js
function Counter(sources) {
    const increment$ = sources.DOM.select('.increment')
        .events('click').mapTo(+1); // On every click on the .increment
                                    // button emit a 1
    
    const decrement$ = sources.DOM.select('.decrement')
        .events('click').mapTo(-1); // Same but with -1

    const state$ = xs.merge(increment$, decrement$)
        .fold((last, curr) => last.curr, 0) // Starting with 0, add up all
                                            // numbers on the stream

    const view$ = state$.map(count => div([
        span(['Count: ' + count]),
        button('.increment'),
        button('.decrement')
    ]));

    return {
        DOM: view$
    };
}
```
But if you call that function twice:
```js
function main(sources) {
    const sink1 = Counter(sources);
    const sink2 = Counter(sources);

    const view$ = xs.combine(sink1.DOM, sink2.DOM)
        .map(children => div(children));

    return {
        DOM: view$
    };
}
```
You get this:

![](./dom-driver/no_isolation.gif)

Why? Because if you take a look at the DOM, you see that there are _two_ elements with the `.increment` class, so either one triggers the emission of events:

![](./dom-driver/dom_no_isolation.svg)

You can solve this issue by using `isolate()` which scopes the events to their components:
```diff
function main(sources) {
-    const sink1 = Counter(sources);
-    const sink2 = Counter(sources);
+    const sink1 = isolate(Counter, 'counter1')(sources);
+    const sink2 = isolate(Counter, 'counter2')(sources);

    const view$ = xs.combine(sink1.DOM, sink2.DOM)
        .map(children => div(children));

    return {
        DOM: view$
    };
}
```

![](./dom-driver/dom_total_isolation.svg)

## Building the bridge between APIs

The goal of us is to build the bridge between the declarative API of the DOM driver including isolation and the native DOM API of the browser.

For this we need to know how the browser processes events. When an event is emitted on an element it first runs through the **capture phase**. This means the event runs top down from the `<html>` to the `<button>` in our case, triggering the event listeners that specified `useCature: true`.

Then, the more well known **bubbling phase**. Now the event runs bottom up through the DOM tree, triggering all event listeners that were not triggered in the capture phase.

So for our isolation we want to stop the events from propagating outside of the current scope. Sadly we can't use `stopPropagation`, because the capture phase always starts at the root of the DOM tree, not the root of our isolation scope.

We want the bubbling phase to look like this:

![](./dom-driver/bubbling_total_isolation.svg)

## Implementing a custom event propagation algorithm

As we already said, we can't use the native event bubbling of the DOM. To make our live a bit easier, we will just attach an native event listener at the root of our cycle app, and use the bubbling to catch all events that happen in the DOM with just one listener (yes, there are events that do not bubble, but I will exclude them for sake of simplicity here).

This root event listener looks like this:
```js
root.addEventListener('click', function(event) {
    const element = event.target;
    // do something
});
```

We know the element where the event happened, but not in which isolation scope this element is, as the DOM does not know anything about isolation. This means we need a mapping from element to isolation scope.

But remember how I said before, the only thing I know about the DOM driver is, that it uses virtual DOM under the hood? How do we get the actual DOM nodes, and not the vnodes?

## Hooking into the VDOM

Snabbdom, the virtual DOM implementation that Cycle.js uses, allows to create modules that can hook into the DOM node create/update/delete livecycle. A basic module looks like this:
```js
const myModule = {
  create: function(emptyVnode, vnode) {
    // invoked whenever a new virtual node is created
    // the actual DOM element is under vnode.elm
  },
  update: function(oldVnode, vnode) {
    // invoked whenever a virtual node is updated
  },
  delete: function(vnode) {
    // invoken whenever a DOM node is removed
  }
};
```

So if we attach the isolation scope information to the vnode, we can use the `create` hook to save the scope together with a reference to the DOM node.

## Attaching the scope information

If we take a look at the `isolate()` API again, we can see that it is a **higher order function**, so a function that takes a function as input and (in our case) returns a new function:
```js
const isolatedComponentFunction = isolate(Component, scope);
```

If we imagine the inner workings of isolate and ignore all other drivers except DOM, it would look a bit like this:
```js
function isolate(Component, scope) {
    return function IsolatedComponent(sources) {
        const isolatedSource = sources.DOM.isolateSource(sources.DOM, scope);
        const sinks = Component({ ...sources, DOM: isolatedSource });

        return {
            ...sinks,
            DOM: sources.DOM.isolateSink(sink.DOM, scope)
        };
    }
}
```
So we have two points of attack, `isolateSource` and `isolateSink`.
