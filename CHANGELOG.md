# Revision history for hyperbole

## 0.6.0 -- 2026-01-15

* `ViewState` - built in threaded state
* `Concurrency` Controls - `Drop` vs `Replace`
* Shiny new documentation: https://hyperbole.live

## 0.5.0 -- 2025-09-26

Improvements
* `trigger` actions in other views
* Javascript FFI
  * `window.Hyperbole` - API available from custom JS. `runAction` allows JS to trigger actions
  * `pushEvent` - send events to JS from the server
* Documents
  * Choose to configure with `View DocumentHead ()` instead of `ByteString` `->` `ByteString`
  * `quickStartDocument`
  * Live Reload
* Websocket - ping keepalive
* New form fields: `radio`, `select`
* `Web.Hyperbole.Effect.OAuth2` - Authentication 
* `Web.Hyperbole.Effect.GenRandom` - Simple random effect used by OAuth2
* Error handling, custom errors
* Examples
  * Many additions and improvements
  * External Stylesheet TodoMVC
  * OAuth2 example

Breaking Changes / Improvements
* `Web.Atomic.CSS` overhauled, and is now opt-in. Use new `@` and `~` operators to apply attributes and styles
* `Web.Hyperbole.Data.Param` - unified param encoding for Forms, ViewId, ViewAction, Sessions, Queries
* `Web.Hyperbole.Data.Encoding` - encoding for ViewId, ViewAction
* `Web.Hyperbole.Data.URI` - Standardize on `Network.URI`, extra utilities to manage paths
* `trigger`: required refactor of `Page` type alias to support type-checking: `Eff es (Page '[])` is now `Page es '[]`

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
