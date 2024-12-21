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

    -- ** View Functions
    -- $view-functions

    -- ** Independent Updates
    -- $multi

    -- ** Examples
    -- $examples

    -- * Run an Application
    liveApp
  , Warp.run
  , runPage
  , basicDocument

    -- ** Type-Safe Routes
  , routeRequest -- maybe belongs in an application section
  , Route (..)
  , routeUrl
  , route

    -- * Create Pages

    -- | Applications are made up of Pages, which have zero dependencies between them. WE start with a page load, etc
  , Page

    -- * Add HyperViews
  , HyperView (..)
  , hyper
  , HasViewId (..)

    -- * Write View Functions

    -- | You need to create a view function for every HyperView

    -- * Interactive Elements

    -- ** Buttons
  , button

    -- ** Live Search
  , search

    -- ** Dropdowns
  , dropdown
  , option
  , Option

    -- ** textarea 
  , textarea
  
    -- ** Events
  , onRequest
  , onLoad
  , DelayMs

    -- * Type-Safe Forms

    -- | Painless forms with type-checked field names, and support for validation. See [Example.Forms](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
  , Form (..)
  , formFields
  , formFieldsWith
  , FormField
  , Field
  , Identity

    -- ** Form View
  , form
  , field
  , label
  , input
  , submit
  , placeholder
  , InputType (..)

    -- ** Handlers

    -- ** Validation
  , Validated (..)
  , validate
  , fieldValid
  , invalidText
  , anyInvalid

    -- * Hyperbole Effect
  , Hyperbole

    -- ** Request Info
  , reqParam
  , reqParams
  , request
  , lookupParam
  , hasParam
  , formBody
  , formData
  , FromHttpApiData

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
  , ViewId
  , ViewAction
  , Response
  , Root
  , HyperViewHandled

    -- * Exports

    -- ** Web.View

    -- | Hyperbole is tightly integrated with [Web.View](https://hackage.haskell.org/package/web-view/docs/Web-View.html) for HTML generation
  , module Web.View

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
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Web.Hyperbole.Application
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (formBody, hasParam, lookupParam, reqParam, reqParams, request)
import Web.Hyperbole.Effect.Respond (notFound, redirect, respondEarly, view)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Embed
import Web.Hyperbole.Forms
import Web.Hyperbole.HyperView
import Web.Hyperbole.Page (Page, runPage)
import Web.Hyperbole.Route
import Web.Hyperbole.View
import Web.View hiding (Query, Segment, button, cssResetEmbed, form, input, label)


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

They are divided into top-level 'Page's, which can run side effects (like loading data), then respond with a 'View'

@
#EMBED docgen/Intro.hs main

#EMBED docgen/Intro.hs messagePage
@
-}


{- $interactive

We can include one or more 'HyperView's to add type-safe interactivity to live subsections of the 'Page'. Define a data type that uniquely identifies it.

@
#EMBED docgen/Intro.hs data Message
@

Make our 'ViewId' an instance of 'HyperView'.

1. Specify every interactive 'Action' possible
2. Write an 'update' function which replaces the contents of the 'HyperView'

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED docgen/Intro.hs instance HyperView Message
@


Embed the new 'HyperView' in the 'Page' using the 'hyper' function, and add our 'ViewId' to the 'Page'. Then add a 'button 'to trigger the 'Action'

@
#EMBED docgen/Intro.hs messagePage'
@

Clicking the button will replace the contents with the result of the 'update'
-}


{- $view-functions

The best way to organize our 'View's is with "View Functions". These are pure functions with state as parameters, returning a View with our 'ViewId' as the 'context'

> #EMBED docgen/Intro.hs messageView

Now we can refactor our 'Page' and 'update' to use the same view function

@
#EMBED docgen/Intro2.hs messagePage

#EMBED docgen/Intro2.hs instance HyperView Message
@
-}


{- $multi

'HyperView's update independently. We can make completely new 'HyperViews', but we can also reuse the same one as long as the value of 'ViewId' is unique. Let's update our 'ViewId' to allow for more than one value.


@
#EMBED docgen/IntroMulti.hs data Message
@

Now we can use the same 'HyperView' multiple times in a page. Each button will update its view independently


@
#EMBED docgen/IntroMulti.hs messagePage
@
-}


{- $examples
The [example directory](https://github.com/seanhess/hyperbole/blob/main/example/README.md) contains an app with pages demonstrating various features

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

-- test :: (Hyperbole :> es) => Page '[Woot, Nope] es ()
-- test =
--   handler woot $ handler nope $ load $ do
--     pure $ do
--       hyper Woot none
--       hyper Nope none
--
--
-- -- makePage
-- --   <$> woot ()
-- --   <*> zoop asdflsadfkl
-- --   <*> do
-- --     pure $ do
-- --       hyper Woot none
-- --       hyper Nope none
--
-- nope :: Nope -> None -> Eff es (View Nope ())
-- nope = _
--
--
-- -- hyper Nope none
--
-- data PageView = PageView
--   deriving (Read, Show, ViewId)
--
--
-- instance HyperView PageView where
--   type Children PageView = '[Woot]
--   type Action PageView = ()
--
--
-- data Woot = Woot
--   deriving (Read, Show, ViewId)
--
--
-- instance HyperView Woot where
--   type Action Woot = None
--   type Children Woot = '[]
--
--
-- woot :: (Hyperbole :> es) => Woot -> None -> Eff es (View Woot ())
-- woot _ _ = pure none
--
--
-- data Nope = Nope
--   deriving (Read, Show, ViewId)
--
--
-- instance HyperView Nope where
--   type Action Nope = None
--
--
-- viewWoot :: View Woot ()
-- viewWoot = do
--   hyper Nope none
--   none
--
-- -- TODO: woot is allowed to appear in our page
-- -- how can we specify this?
-- -- certain views are allowed in others?
--

{-

#EMBED docgen/Intro.hs main

#EMBED docgen/Intro.hs messagePage

#EMBED docgen/Intro.hs data Message

#EMBED docgen/Intro.hs instance HyperView Message

> #EMBED docgen/Intro.hs messageView

-}
