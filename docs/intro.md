Single Page Applications (SPAs) require the programmer to write two programs: a Javascript client and a Server, which both must conform to a common API


Hyperbole allows us to instead write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

>  Why write HTML and Javascript when you can... not do that? 

There are frameworks that support this in different ways, including [HTMX](https://htmx.org/), [Phoenix LiveView](https://www.phoenixframework.org/), and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe views, actions, routes, and forms
3. Elegant interface with little boilerplate
4. VirtualDOM updates over sockets
5. Easy to use

>  1000x more fun than React!

Like [HTMX](https://htmx.org/), Hyperbole extends the capability of UI elements, but it uses Haskell's type-system to prevent common errors and provide default functionality. Specifically, a page has multiple update targets called `HyperView`s. These are automatically targeted by any UI element that triggers an action inside them. The compiler makes sure that actions and targets match

Like [Phoenix LiveView](https://www.phoenixframework.org/), it upgrades the page to a fast WebSocket connection and uses VirtualDOM for live updates

Like [Elm](https://elm-lang.org/), it uses an `update` function to process actions, but greatly simplifies the Elm Architecture by directly returning html instead of using a reducer. `ViewState` is optional. Effects are handled by [Effectful](https://hackage.haskell.org/package/effectful). `form`s are easy to use with minimal boilerplate

Hyperbole depends heavily on the following frameworks:

* [Effectful](https://hackage.haskell.org/package/effectful)
* [Atomic CSS](https://hackage.haskell.org/package/atomic-css)

<!-- >  Accidentally learn to use extensible effects! -->
