# Revision history for hyperbole

## 0.5.0 -- _____

* Major improvements to examples
* error handling, custom errors
* Web.Hyperbole.Effect.OAuth2
* Web.Hyperbole.Effect.GenRandom
* Web.Hyperbole.Data.URI
* OAuth2 example
* `trigger` actions in other views
  * required refactor of `Page` type alias to support type-checking: `Eff es (Page '[])` is now `Page es '[]`
* Javascript pushEvent
* Live reload
* quickStartDocument
* View DocumentHead ()
* ping keepalive

## 0.4.3 -- 2025-01-31

* Bug fixes and improvements

## 0.4.2 -- 2025-01-21

* Cleaner HyperView class [(@cgeorgii)](https://github.com/cgeorgii)
  * data family Action
  * update
* Type-safe resolution of HyperViews
* Record-based Forms
  * textarea [(@tusharad)](https://github.com/tusharad)
* High-level sessions and query params
* Events: onLoad, onClick onInput, onSubmit, onDblClick, onKeyDown, onKeyUp
* Major refactoring
* Nix build and CI [(@Skyfold)](https://github.com/Skyfold)
* New Examples Live: https://docs.hyperbole.live
* New Examples Added:
  * TodoMVC
  * Forms - Simple
  * DataTable
  * Search - Filters
  * Search - Autocomplete

## 0.3.6 -- 2024-05-21

* First version. Released on an unsuspecting world.
