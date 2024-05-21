{- |
Module:      Web.Hyperbole
Copyright:   (c) 2024 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Create fully interactive HTML applications with type-safe serverside Haskell. Inspired by [HTMX](https://htmx.org/), [Elm](https://elm-lang.org/), and [Phoenix LiveView](https://www.phoenixframework.org/)
-}
module Web.Hyperbole
  ( -- * Introduction
    -- $use

    -- ** Hello World
    -- $hello

    -- ** Interactivity
    -- $interactive

    -- ** Independent Updates
    -- $multi

    -- ** Examples
    -- $examples

    -- * Run an Application
    liveApp
  , Warp.run
  , page
  , basicDocument

    -- ** Type-Safe Routes
  , routeRequest -- maybe belongs in an application section
  , Route
  , routeUrl
  , route

    -- * Pages

    -- ** Page
  , Page
  , load
  , handle

    -- ** HyperView
  , HyperView (..)
  , hyper

    -- * Interactive Elements

    -- ** Buttons
  , button

    -- ** Dropdowns
  , dropdown
  , option
  , Option

    -- ** Events
  , onRequest
  , onLoad
  , DelayMs

    -- * Type-Safe Forms

    -- | Painless forms with type-checked field names, and support for validation. See [Example.Forms](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
  , FormField

    -- ** Form View
  , form
  , field
  , label
  , input
  , submit
  , placeholder
  , InputType (..)

    -- ** Handlers
  , formField

    -- ** Validation
  , Validation (..)
  , validate
  , validation
  , invalidText

    -- * Hyperbole Effect
  , Hyperbole

    -- ** Request Info
  , reqParam
  , reqParams
  , request
  , lookupParam
  , formData

    -- ** Response
  , notFound
  , redirect
  , respondEarly

    -- ** Sessions
  , session
  , setSession
  , clearSession

    -- * Advanced
  , target
  , view
  , Param (..)
  , Response

    -- * Exports

    -- ** Web.View

    -- | Hyperbole is tightly integrated with [Web.View](https://hackage.haskell.org/package/web-view/docs/Web-View.html) for HTML generation
  , module Web.Hyperbole.View

    -- ** Embeds

    -- | Embedded CSS and Javascript to include in your document function. See 'basicDocument'
  , module Web.Hyperbole.Embed

    -- ** Effectful
    -- $effects
  , module Effectful

    -- ** Other
  , Application
  , Generic
  ) where

import Effectful (Eff, (:>))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Web.Hyperbole.Application
import Web.Hyperbole.Effect
import Web.Hyperbole.Embed
import Web.Hyperbole.Forms (FormField, InputType (..), Validation (..), field, form, formField, input, invalidText, label, placeholder, submit, validate, validation)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.Hyperbole.View


-- import Web.View.Types.Url (Segment)

{- $use

Single Page Applications (SPAs) require the programmer to write two programs: a Javascript client and a Server, which both must conform to a common API

Hyperbole allows us instead to write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in different ways, including [HTMX](https://htmx.org/), [Phoenix LiveView](https://www.phoenixframework.org/), and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe views, actions, routes, and forms
3. Elegant interface with little boilerplate
4. VirtualDOM updates over sockets, fallback to HTTP
5. Easy to use

Like [HTMX](https://htmx.org/), Hyperbole extends the capability of UI elements, but it uses Haskell's type-system to prevent common errors and provide default functionality. Specifically, a page has multiple update targets called 'HyperView's. These are automatically targeted by any UI element that triggers an action inside them. The compiler makes sure that actions and targets match

Like [Phoenix LiveView](https://www.phoenixframework.org/), it upgrades the page to a fast WebSocket connection and uses VirtualDOM for live updates

Like [Elm](https://elm-lang.org/), it relies on an update function to 'handle' actions, but greatly simplifies the Elm Architecture by handling state with extensible effects. 'form's are easy to use with minimal boilerplate

Depends heavily on the following frameworks

* [Effectful](https://hackage.haskell.org/package/effectful-core)
* [Web View](https://hackage.haskell.org/package/web-view)
-}


{- $hello

Hyperbole applications run via [Warp](https://hackage.haskell.org/package/warp) and [WAI](https://hackage.haskell.org/package/wai)

They are divided into top-level 'Page's. We use 'load' to handle the initial page load

@
main = do
  'run' 3000 $ do
    'liveApp' ('basicDocument' \"Example\") ('page' messagePage)

messagePage = do
  'load' $ do
    pure $ do
      'el' 'bold' "Message Page"
      messageView "Hello World"

messageView m = do
  el_ "Message:"
  el_ (text m)
@
-}


{- $interactive

Embed 'HyperView's to add type-safe interactivity to subsections of a 'Page'.

To do this, first we connect a view id type to the actions it supports

@
data Message = Message
  deriving (Generic, 'Param')

data MessageAction = Louder Text
  deriving (Generic, 'Param')

instance 'HyperView' Message where
  type Action Message = MessageAction
@


Next we add a 'handle'r for our view type. It performs side effects, and returns a new view of the same type

@
message :: Message -> MessageAction -> Eff es (View Message ())
message _ (Louder m) = do
  -- side effects
  let new = m <> "!"
  pure $ messageView new
@

We update our parent page view to make the messageView interactive using 'hyper', and add our 'handle'r to the 'Page'

@
messagePage = do
  'handle' message
  'load' $ do
    pure $ do
      'el' 'bold' "Message Page"
      'hyper' Message $ messageView "Hello World"
@

Finally, let's add a 'button' to our view. When clicked, Hyperbole will run the `message` handler, and update our view, leaving the page header untouched

@
messageView :: Text -> 'View' Message ()
messageView m = do
  'el_' m
  'button' (Louder m) id "Change Message"
@
-}


{- $multi

Multiple views update independently, as long as they have different values for their View Id. Add an Int identifier to Message

@
data Message = Message Int
  deriving (Generic, 'Param')
@

We can embed multiple HyperViews on the same page with different ids. Each button will update its view independently


@
messagePage = do
  'handle' message
  'load' $ do
    pure $ do
      'el' bold "Message Page"
      'hyper' (Message 1) $ messageView \"Hello\"
      'hyper' (Message 2) $ messageView \"World\"
@
-}


{- $examples
The [example directory](https://github.com/seanhess/hyperbole/blob/main/example/example/README.md) contains an app with pages demonstrating various features

* [Main](https://github.com/seanhess/hyperbole/blob/main/example/Main.hs)
* [Simple](https://github.com/seanhess/hyperbole/blob/main/example/Example/Simple.hs)
* [Counter](https://github.com/seanhess/hyperbole/blob/main/example/Example/Counter.hs)
* [CSS Transitions](https://github.com/seanhess/hyperbole/blob/main/example/Example/Transitions.hs)
* [Forms](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
* [Sessions](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
* [Redirects](https://github.com/seanhess/hyperbole/blob/main/example/Example/Redirects.hs)
* [Lazy Loading and Polling](https://github.com/seanhess/hyperbole/blob/main/example/Example/LazyLoading.hs)
* [Errors](https://github.com/seanhess/hyperbole/blob/main/example/Example/Errors.hs)
* [Contacts (Advanced)](https://github.com/seanhess/hyperbole/blob/main/example/Example/Contacts.hs)
-}


{- $effects

Hyperbole is tighly integrated with [Effectful](https://hackage.haskell.org/package/effectful) for extensible effects. It is used to implement the 'Hyperbole' and 'Server' effects.

* See [Effectful.Dispatch.Dynamic](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) for an example of how to create a custom effect
* See [Example.Counter](https://github.com/seanhess/hyperbole/blob/main/example/Example/Counter.hs) for an example of how to compose an existing effect
-}
