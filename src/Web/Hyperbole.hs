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

    -- ** Examples
    -- $examples

    -- * Getting started
    -- $hello

    -- ** Interactivity
    -- $interactive

    -- ** View Functions
    -- $view-functions

    -- * Managing State
    -- $state-parameters

    -- ** Side Effects
    -- $state-effects

    -- * Design and Best Practices

    -- ** Reusable styles
    -- $practices-styles

    -- ** Reusable View Functions
    -- $practices-view-functions

    -- ** Different HyperViews
    -- $practices-multi

    -- ** Reusing the same HyperView
    -- $practices-same

    -- ** Nested HyperViews
    -- $practices-nested

    -- * Application
    liveApp
  , Warp.run
  , runPage
  , basicDocument

    -- ** Type-Safe Routes
  , routeRequest -- maybe belongs in an application section
  , Route (..)
  , routeUrl
  , route

    -- * Pages

    -- | Applications are made up of Pages, which have zero dependencies between them. WE start with a page load, etc
  , Page

    -- * HyperViews
  , HyperView (..)
  , hyper
  , HasViewId (..)

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

Hyperbole allows us to instead write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in different ways, including [HTMX](https://htmx.org/), [Phoenix LiveView](https://www.phoenixframework.org/), and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe views, actions, routes, and forms
3. Elegant interface with little boilerplate
4. VirtualDOM updates over sockets, fallback to HTTP
5. Easy to use

Like [HTMX](https://htmx.org/), Hyperbole extends the capability of UI elements, but it uses Haskell's type-system to prevent common errors and provide default functionality. Specifically, a page has multiple update targets called 'HyperView's. These are automatically targeted by any UI element that triggers an action inside them. The compiler makes sure that actions and targets match

Like [Phoenix LiveView](https://www.phoenixframework.org/), it upgrades the page to a fast WebSocket connection and uses VirtualDOM for live updates

Like [Elm](https://elm-lang.org/), it uses an update function to process actions, but greatly simplifies the Elm Architecture by remaining stateless. Effects are handled by [Effectful](https://hackage.haskell.org/package/effectful). 'form's are easy to use with minimal boilerplate

Hyperbole depends heavily on the following frameworks

* [Effectful](https://hackage.haskell.org/package/effectful-core)
* [Web View](https://hackage.haskell.org/package/web-view)
-}


{- $hello

Hyperbole applications run via [Warp](https://hackage.haskell.org/package/warp) and [WAI](https://hackage.haskell.org/package/wai)

They are divided into top-level 'Page's, which can run side effects (like loading data), then respond with a 'View'

@
{\-# LANGUAGE DeriveAnyClass #-\}
{\-# LANGUAGE OverloadedStrings #-\}
{\-# LANGUAGE TypeFamilies #-\}

module Main where

#EMBED Example/Intro/BasicPage.hs main

#EMBED Example/Intro/BasicPage.hs messagePage
@
-}


{- $interactive

We can include one or more 'HyperView's to add type-safe interactivity to live subsections of the 'Page'. Define a data type that uniquely identifies it.

@
#EMBED Example/Intro/Interactive.hs data Message
@

Make our 'ViewId' an instance of 'HyperView' by:

* Create a constructor for every 'Action' possible
* Write an 'update' for each one

@
#EMBED Example/Intro/Interactive.hs instance HyperView Message
@


Embed our new 'HyperView' in the 'Page' with 'hyper', and add our 'ViewId' to the 'Page' type signature. Then add a 'button' to trigger the 'Action'

@
#EMBED Example/Intro/Interactive.hs messagePage
@

Now clicking the button will replace the contents with the result of the 'update'
-}


{- $view-functions

To avoid repeating ourselves, we can organize our 'View's into __View Functions__. These are pure functions with state as parameters, which return a 'View'.

@
#EMBED Example/Intro/ViewFunctions.hs messageView
@

Now we can refactor our 'Page' and 'update' to use the same view function. Only the message will change.

@
#EMBED Example/Intro/ViewFunctions.hs messagePage

#EMBED Example/Intro/ViewFunctions.hs instance HyperView Message
@
-}


{- $practices-multi

We can add as many 'HyperView's to a page as we want. Let's add a simple counter 'HyperView'

@

#EMBED Example/Intro/MultiView.hs data Count


#EMBED Example/Intro/MultiView.hs instance HyperView Count


#EMBED Example/Intro/MultiView.hs countView
@

We can use both 'Message' and 'Count' 'HyperView's in our page, and they will update independently:

@
#EMBED Example/Intro/MultiView.hs page
@
-}


{- $practices-same

In addition to creating different 'HyperView's, we can reuse the same one as long as the value 'ViewId' is unique. Let's update `Message` to allow for more than one value:


@
#EMBED Example/Simple.hs data Message
@

Now we can embed multiple `Message` 'HyperView's into the same page. Each button will update its view independently. See [Example.Simple](https://docs.hyperbole.live/simple) to view this example

@
#EMBED Example/Simple.hs messagePage
@


This is especially useful if we put identifying information in our 'ViewId', such as a database id. [Example.Contacts](https://docs.hyperbole.live/contacts) uses this to allow the user to edit multiple contacts on the same page. The 'viewId' function gives us access to the info in our 'update'

@
#EMBED Example/Contact.hs data Contact


#EMBED Example/Contact.hs instance (Users :> es,
@
-}


{- $practices-nested

We can nest smaller, specific 'HyperView's inside of a larger parent. You might need this technique to display a list of items, while making each item editable.
-}


{- $practices-styles

TODO: directly reusing styles
-}


{- $practices-view-functions

You may be tempted build HyperViews into generic "Components". This leads to inheretence-like designs that don't compose well. We are using a functional language, so our main unit of reuse should be functions!

For example, let's say you wanted a reusable toggle button on user preferences page. It would be messy and complex to make each toggle a separate 'HyperView'. Instead, let's turn it into a view function

TODO: components for reusing styles, like a toggle button
-}


{- $examples
Visit https://docs.hyperbole.live to see an application demonstrating various features on different pages. Each example includes a link to the source code
-}


{- $state-parameters

'HyperView's are stateless. They 'update' based entirely on the 'Action'. However, we can track simple state by passing it back and forth between the 'Action' and the view function

@
#EMBED Example/Intro/State.hs instance HyperView Message


#EMBED Example/Intro/State.hs messageView
@
-}


{- $state-effects

For any real application with more complex state and data persistence, we need side effects.

Hyperbole relies on [Effectful](https://hackage.haskell.org/package/effectful) to compose side effects. We can use effects in a 'Page' or an 'update'. In this example, each client stores the latest message in their session.

@
#EMBED Example/Intro/SideEffects.hs messagePage

#EMBED Example/Intro/SideEffects.hs instance HyperView Message
@

* See [Example.Counter](https://docs.hyperbole.live/counter) for an example of how to compose effects to create a shared state for all users
* See [Effectful.Dispatch.Dynamic](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) for an example of how to create a custom effect

To use an 'Effect' other than 'Hyperbole', add it as a constraint to the 'Page' and any 'HyperView' instances that need it. Then run the effect in your application






* See [NSO's Level2 ] ://github.com/DKISTDC/level2/blob/main/src/NSO/Data/Datasets.hs




-}


{- $design

** Item

Hello

** Item 2

Hello
-}
