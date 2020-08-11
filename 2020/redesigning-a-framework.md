# Redesigning a framework

For the last few years, the core team of Cycle.js ([André](https://twitter.com/andrestaltz) and me) has been redesigning the architecture and the developer experience of the framework. This February we finally found a solution to our problems that still stays true to the core ideas of the framework.

This blog post marks the first in a series that will cover the new design and its development. In this installment, I want to bring everyone onto the same page. _What_ where the problems I described earlier and _how_ does the new design solve them. In the later articles I will cover the new `run` function (the core of the framework) and the new HTTP driver and especially the issues I encountered while implementing those. _\*cough\*_ race conditions _\*cough\*_.

## The status quo

Everyone that is familiar with Cycle.js may skip this part, for the rest here is how the framework works in the current version: Everything in your application is based around the notion of streams. The kinds of streams that RxJS made popular. All you application code is doing is reading streams of events from the outside (ie click events on the DOM or responses of HTTP requests), transforming and combining them and finally giving streams of commands back to the outside (ie a new virtual DOM to render on the DOM or a HTTP request to execute).

Let's take a concrete example, a simple counter:

```js
function main(sources) {
    const incrementStream = sources.DOM.select(".increment")
        .events("click")
        .mapTo(1);

    const decrementStream = sources.DOM.select(".decrement")
        .events("click")
        .mapTo(-1);

    const valueStream = xs
        .merge(incrementStream, decrementStream)
        .fold((sum, current) => sum + current, 0);

    const domStream = valueStream.map(x =>
        div([
            h2(`The current value is ${x}`),
            button(".increment", "Increment"),
            button(".decrement", "Decrement")
        ])
    );

    return {
        DOM: domStream
    };
}
```

As you can see we are listing to the click events of the two buttons and convert those events into `+1` and `-1`. We then `merge` those two streams and use `fold` to sum up all numbers (`fold` is similar to `array.fold`, but instead of calculating the value once, `fold` will send out the current value after every number that comes in). We then take the stream of all the sums and transform it into a virtual dom tree that is then given to the outside for rendering.

This stream-centric design has some nice benefits. First, all of the application logic is a pure function. It does not directly access the DOM API, it does not do HTTP requests to 3rd parties or do any other interaction with the outside world. Everything happens through the sources and the sinks (ie input and output of the `main` function). This means that we do not need to mock the actual APIs with something like JsDOM, we can just provide some inputs to the application and assert on the outputs. Second, adding async behavior does not add any complexity, synchronous code looks exactly like asynchronous code. Third, on the top level, we can intercept and modify/filter/log any command that any component in the hierarchy sent. One nice use case for this intercepting every HTTP request the components do and add some API token to the headers for example. We could also add some rate limiting here in case we are fetching from a third party API. We could also put this functionality in a library that provides a function that wraps you application and returns a new application with logging. This pattern has evolved out of the community and there are several libraries that provide such "main wrappers". Finally there is only unidirectional data flow. All the data comes in from the sources, gets transformed and leaves through the sinks. It is really easy to trace commands back to the data or events that caused them.

## The problem

The streaming idea works really well if the outside is interactive, for example it is a really good approach for the DOM where the user may interact at any time. However there is also another kind of outside: question and answer style effects. The most simple example for this is doing HTTP requests. Usually when you send a request, you want to wait for the result and then work with the data. But at the moment doing a request looks like this:

```ts
function main(sources) {
    const responseStream = sources.HTTP.select("myRequest");

    const domStream = responseStream.startWith(initialData).map(view);

    const requestStream = sources.DOM.select(".requestButton")
        .events("click")
        .mapTo({
            url: myUrl,
            method: "GET",
            category: "myRequest"
        });

    return {
        DOM: domStream,
        HTTP: requestStream
    };
}
```

As you can see, while the flow of the data is still strictly from the sources to the sinks, the code is awkward to read for the HTTP portion. First, we listen to a response with some tag (`myRequest` in this case) and then only later we see the code that actually sent it. And they are not directly connected, they are completely independent, so you have to use the tag to find which request belongs to which response. What we really wanted, was an API similar to this:

```js
function main(sources) {
    const domStream = sources.DOM.select(".requestButton")
        .events("click")
        .map(() => sourcs.HTTP.get(myUrl))
        .flatten()
        .startWith(initialData)
        .map(view);

    return {
        DOM: domStream
    };
}
```

This code does exactly the same as the one before, but it is a lot easier to read because you can just start at the top and work your way down. It clearly says: "Listen to all 'click' events on the request button and for each of the clicks make a get request to `myUrl`. Starting with some initial data, render every response with the view function onto the DOM".

But if we would implement this like that, we would loose one of the benefits of using stream: The ability to inspect and modify every command that happens. As you can see there is nothing returned through the sinks for HTTP, so we can not intercept this request anywhere, not even at the top.

## The solution

The solution we have settled on now is to split the drivers that interpret the commands and provide the events. At the moment a driver take a stream of commands as input and returns either a stream of events or, for more complex drivers like HTTP and DOM, an object that provides methods that return streams. For example the DOM driver returns the `DOMSource` object that provides the methods `select()` and `events()` where the latter one returns a stream of events.

A very simplified example of this would look like this:

```ts
class DOMSource {
    events(type) {
        return fromEvent(type);
    }
}

function domDriver(commands) {
    commands.subscribe({
        next: renderDOM
    });

    return new DOMSource();
}
```

In this example `fromEvent` would attach an event listener and emit a new event every time the event listener gets activated.

The new solution changes this to require drivers to take a stream as input and return a stream as output. If a more complex driver wants to offer a nicer API it can provide it separately. The job of such an API is to convert the calls from the user into commands that will be sent to the driver and to take the events from the driver and filter them for the user. For our DOM example this might look like this:

```ts
class DomApi {
    constructor(subject, driverEvents, idGenerator) {
        this.subject = subject;
        this.driverEvents = driverEvents;
        this.idGenerator = idGenerator;
    }

    events(type) {
        const id = this.idGenerator();
        this.subject.send({
            commandType: "attachEventListener",
            type,
            id
        });

        return this.driverEvents.filter(event => event.id === id);
    }
}

function domDriver(commands) {
    const subject = makeSubject();

    commands.subscribe({
        next: command => {
            if (command.commandType === "attachEventListener") {
                document.addEventListener(command.type, event => {
                    subject.send({ ...event, id: command.id });
                });
            } else {
                renderDOM();
            }
        }
    });

    return subject;
}
```

As you can see, the driver is completely independent from the API, you could also not use the API and send the commands to the driver directly. The API on the other hand does not interact with the outside world at all, it only sends a command to the driver and filters the events for the ones the user is actually interested in. In case you are wondering, a subject is like the beginning of a stream where you can manually put events into the stream via `send()`.

## The whole picture

With the new design, Cycle.js exports a function `makeMasterMain()` that takes your application and the APIs of the drivers and returns a new main function that just expects streams of events as inputs and returns streams of commands. The APIs of the drivers take care of sending out the right commands and reading the right events. You can now wrap that new main function with code that inspects for example the HTTP requests. But now such code could also intercept and log the addition of event listeners to the DOM! This was not possible before. After adding as many layers of wrapping code to the master main as you like, you can give it to `run()` that takes the main function and the drivers and connects the two. Remember that the main function now only works with plain streams, no APIs any more.

So, coming back to the code from earlier:

```ts
function main(sources) {
    const domStream = sources.DOM.select(".requestButton")
        .events("click")
        .map(() => sourcs.HTTP.get(myUrl))
        .flatten()
        .startWith(initialData)
        .map(view);

    return {
        DOM: domStream
    };
}
```

This is how the code will actually look like in the next major version of Cycle.js! All while you will still be able to intercept/modify/log all requests that leave your application even though they are not explicitly returned from your application code (ie no `HTTP: requestStream`). Getting to this point took some time, but I am very happy with the final architecture. The user code is easier to read and the framework code also got quite a bit simpler.

In the next part I will talk about the `run()` and the `makeMasterMain()` functions and how to prevent race conditions with synchronous stream code. Thanks for reading, feel free to voice any questions you might have.
