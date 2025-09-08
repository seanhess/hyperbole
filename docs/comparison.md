Comparison with Similar Frameworks
==================================

[HTMX](https://htmx.org/)
----------------

Similarities

* Updates return new views

Differences

* Typed HyperViews provide structure to a page
* Actions and updates grouped together by HyperView
* Limited swap options and other advanced features in the name of preserving developer sanity


[Elm](https://elm-lang.org/)
---------------------------

Similarities

* ADTs of Actions with corresponding updates
* Encourages using view functions

Differences

* Runs serverside
* Greatly simplifies the Elm Architecture by removing state. Updates directly return views
* Simpler forms
* Side effects are handled by [Effectful](https://hackage.haskell.org/package/effectful)
* Page and nested view hierarchy are more intuitive and require less boilerplate


[Phoenix LiveView](https://www.phoenixframework.org/)
------------------------------------------------------

Similarities

* Upgrades the page to a fast WebSocket connection
* Patches the page in-place using VDOM
* Client-side events
