# Revision history for hyperbole


## 0.7.1 -- 2026-06-29

Features:
* Accept multipart form file uploads with good default security settings

Improvements:
* `trigger` and `pushEvent` now send to the client immediately instead of waiting for the action to finish
* Added `cookieSecure` option to Session class
* Deprecated `toActionInput` in favor of `inputValue` - Instead of partially applying a constructor, inputs are read with a function similar to forms
* `Web.Hyperbole.Data.Param` was unnecessary and removed. Everything uses ToJSON and FromJSON as an intermediate encoding
* moved OAuth2 to sub-package to allow for dependencies on cryptography libraries

Breaking Changes:
* Actions with input like `search`, `onInput` and `dropdown` should now use an action with no arguments. Get the value using `inputValue` instead
* `ToParam` and `FromParam` are no longer used. Use the generic implementation of `ToJSON` and `FromJSON` instead

Maintenance:
* NIX support is fully up to date
* Strict Typescript
* [Many issues resolved](https://github.com/seanhess/hyperbole/issues?q=is%3Aissue%20state%3Aclosed)



## 0.6.0 -- 2026-01-15

Improvements:
* `ViewState` - built in threaded state, defaults to `()`, for folks who really miss Elm
* `Concurrency` Controls - `Drop` vs `Replace` for overlapping updates
* `pushUpdate` - server push an update to an arbitrary view
* Long-running actions can be interrupted / cancelled
* https://hyperbole.live now has inline documentation, code snippets, and live examples

Breaking Changes:
* A few functions now require state, such as `trigger` and `target`

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
