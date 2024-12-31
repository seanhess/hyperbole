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

    -- * Getting started
    -- $hello

    -- ** Interactive HyperViews #hyperviews#
    -- $interactive

    -- ** View Functions #viewfunctions#
    -- $view-functions

    -- * Managing State
    -- $state-parameters

    -- ** Side Effects
    -- $state-effects

    -- ** Databases and Custom Effects
    -- $state-databases

    -- * Multiple HyperViews
    -- $practices-multi

    -- ** Copies
    -- $practices-same

    -- ** Nesting
    -- $practices-nested

    -- * Components #components#
    -- $components

    -- * Pages #pages#
    -- $practices-pages

    -- * Examples
    -- $examples

    -- * Application
    liveApp
  , Warp.run
  , basicDocument
  , runPage
  , Page

    -- ** Type-Safe Routes
  , routeRequest -- maybe belongs in an application section
  , Route (..)
  , routeUrl
  , route

    -- * Hyperbole Effect
  , Hyperbole

    -- ** Request
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

    -- * HyperView
  , HyperView (..)
  , hyper
  , HasViewId (..)

    -- * Events
  , onClick
  , onDblClick
  , onInput
  , onKeyDown
  , onKeyUp
  , onLoad
  , onRequest
  , Key (..)
  , DelayMs

    -- * Interactive Elements
  , button
  , search
  , dropdown
  , option
  , Option

    -- * Type-Safe Forms

    -- | Painless forms with type-checked field names, and support for validation. See [Example.Forms](https://docs.hyperbole.live/forms)
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
  , textarea
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
  , module Web.Hyperbole.View.Embed

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
import Web.Hyperbole.HyperView
import Web.Hyperbole.Page (Page, runPage)
import Web.Hyperbole.Route
import Web.Hyperbole.View
import Web.Hyperbole.View.Embed
import Web.Hyperbole.View.Forms
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
{\-# LANGUAGE DataKinds #-\}

module Main where

import Web.Hyperbole

#EMBED Example/Intro/BasicPage.hs main

#EMBED Example/Intro/BasicPage.hs messagePage
@
-}


{- $interactive

We can include one or more 'HyperView's to add type-safe interactivity to live subsections of the 'Page'. To start, first define a data type (a 'ViewId') that uniquely identifies that subsection of the page:

@
#EMBED Example/Intro/Interactive.hs data Message
@

Make our 'ViewId' an instance of 'HyperView' by:

* Create an 'Action' type with a constructor for every possible way that the user can interact with it
* Write an 'update' for each 'Action'

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

Rather than create a completely different 'View' on each update, we can create a __View Function__ for our 'HyperView'. These are pure functions with state parameters, which return a 'View'. The compiler will tell us if we try to trigger an 'Action' for a 'HyperView' that doesn't match our 'View' 'context'

It's helpful to create a main view function for each 'HyperView':

@
#EMBED Example/Intro/ViewFunctions.hs messageView
@

Now we can refactor to use the main view function for both our 'update' and where we embed it in the 'Page' with 'hyper'. The only thing that will change on an update is the text of the message.

@
#EMBED Example/Intro/ViewFunctions.hs messagePage

#EMBED Example/Intro/ViewFunctions.hs instance HyperView Message
@

We can create multiple view functions with our 'HyperView' as the 'context', and factor them however is most convenient.

@
#EMBED Example/Intro/ViewFunctions.hs goodbyeButton
@

We can also create view functions that work in any context. We will talk more about these `Components` below.

@
#EMBED Example/Intro/ViewFunctions.hs header
@

Factored this way, our main 'View' for 'Message' becomes:

@
#EMBED Example/Intro/ViewFunctions.hs messageView'
@
-}


{- $practices

We've mentioned most of the Architecture of a hyperbole application, but let's go over each layer here:

* [Pages](#g:pages) - routes map to completely independent web pages
* [HyperViews](#g:hyperviews) - independently updating live subsections of a 'Page'
* Main [View Functions](#g:viewfunctions) - A view function that renders and updates a 'HyperView'
* [Components](#g:components) - Generic view functions you can use in any 'HyperView'
-}


{- $practices-multi

We can add as many 'HyperView's to a page as we want. Let's create another 'HyperView' for a simple counter

From [Example.Intro.MultiView](https://github.com/seanhess/hyperbole/blob/latest/example/Example/Intro/MultiView.hs)

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

We can embed multiple copies of the same 'HyperView' as long as the value of 'ViewId' is unique. Let's update `Message` to allow for more than one value:

See [Example.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Simple.hs data Message
@

Now we can embed multiple `Message` 'HyperView's into the same page. Each will update independently.

@
#EMBED Example/Simple.hs messagePage
@


This is especially useful if we put identifying information in our 'ViewId', such as a database id. The [Contacts Example](https://docs.hyperbole.live/contacts) uses this to allow the user to edit multiple contacts on the same page. The 'viewId' function gives us access to that info

From [Example.Contacts](https://docs.hyperbole.live/contacts)

@
#EMBED Example/Contact.hs data Contact

#EMBED Example/Contact.hs instance (Users :> es,
@
-}


{- $practices-pages

An app has multiple 'Page's with different 'Route's that each map to a unique url path:

@
#EMBED Example/Intro/MultiPage.hs data AppRoute
@

When we define our app, we define a function that maps a 'Route' to a 'Page'

@
#EMBED Example/Intro/MultiPage.hs main
@

Each 'Page' is completely independent. The web page is freshly reloaded each time you switch routes. We can add type-safe links to other pages using 'route'

@
#EMBED Example/Intro/MultiPage.hs menu
@

If you need the same header or menu on all pages, use a component:

@
#EMBED Example/Intro/MultiPage.hs exampleLayout

#EMBED Example/Intro/MultiPage.hs examplePage
@

As shown above, each 'Page' can contain multiple interactive 'HyperView's to add interactivity
-}


{- $practices-nested

We can nest smaller, specific 'HyperView's inside of a larger parent. You might need this technique to display a list of items which need to update themselves

Let's imagine we want to display a list of Todos. The user can mark individual todos complete, and have them update independently. The specific 'HyperView' might look like this:

From [Example.Intro.Nested](https://github.com/seanhess/hyperbole/blob/latest/example/Example/Intro/Nested.hs)

@
#EMBED Example/Intro/Nested.hs data TodoItem

#EMBED Example/Intro/Nested.hs instance HyperView TodoItem
@

But we also want the entire list to refresh when a user adds a new todo. We need to create a parent 'HyperView' for the whole list.

List all allowed nested views by adding them to 'Require'

@
#EMBED Example/Intro/Nested.hs data AllTodos

#EMBED Example/Intro/Nested.hs instance HyperView AllTodos
@

Then we can embed the child 'HyperView' into the parent with 'hyper'

@
#EMBED Example/Intro/Nested.hs todosView
@
See this technique used in the [TodoMVC Example](https://docs.hyperbole.live/todos)
-}


{- $components

We showed earlier how to create a generic [View Function](#g:view-functions) that we can reuse in any view. We call these __Components__

@
#EMBED Example/Intro/ViewFunctions.hs header
@

This component doesn't do anything yet. More complex commponents will need to trigger 'Action's. You may be tempted to make a super-generic 'HyperView'. This leads to inheretence-like designs that don't compose well. We are using a functional language, so our main unit of reuse should be functions!

So how can we create a function that triggers actions for different 'HyperView's? Pass the 'Action' into the component as a parameter:

@
#EMBED Example/Intro/Component.hs styledButton
@

We can create more complex components by passing state in as a parameter. Here's a button that toggles between a checked and unchecked state:

@
#EMBED Example/View/Inputs.hs toggleCheckBtn
@

Don't leverage 'HyperView's for code reuse. Think about which subsections of a page ought to update independently. Those are 'HyperView's. If you need reusable functionality, use `Components` instead.
-}


{- $examples
https://docs.hyperbole.live is full of live examples demonstrating different features. Each example includes a link to the source code. Some highlights:

* [Simple](https://docs.hyperbole.live/simple)
* [Counter](https://docs.hyperbole.live/counter)
* [CSS Transitions](https://docs.hyperbole.live/transitions)
* [Lazy Loading](https://docs.hyperbole.live/lazyloading)
* [Filter Items](https://docs.hyperbole.live/filter)
* [Autocomplete](https://docs.hyperbole.live/livesearch)
* [Todo MVC](https://docs.hyperbole.live/todos)

The [National Solar Observatory](https://nso.edu) uses Hyperbole for the Level 2 Data creation tool for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/). It is completely [open source](https://github.com/DKISTDC/level2/). This production application contains complex interfaces, workers, databases, and more.
-}


{- $state-parameters

'HyperView's are stateless. They 'update' based entirely on the 'Action'. However, we can track simple state by passing it back and forth between the 'Action' and the 'View'

From [Example.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Intro/State.hs instance HyperView Message

#EMBED Example/Intro/State.hs messageView
@
-}


{- $state-effects

For any real application with more complex state and data persistence, we need side effects.

Hyperbole relies on [Effectful](https://hackage.haskell.org/package/effectful) to compose side effects. We can use effects in a 'Page' or an 'update'. In this example, each client stores the latest message in their session.

From [Example.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Intro/SideEffects.hs messagePage

#EMBED Example/Intro/SideEffects.hs instance HyperView Message
@


To use an 'Effect' other than 'Hyperbole', add it as a constraint to the 'Page' and any 'HyperView' instances that need it. Then run the effect in your application

From [Example.Counter](https://docs.hyperbole.live/counter)

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Counter.hs instance (Reader
@
-}


{- $state-databases

A database is no different from any other 'Effect'. We recommend you create a custom effect to describe high-level data operations.

From [Example.Effects.Todos](https://github.com/seanhess/hyperbole/blob/latest/example/Example/Effects/Todos.hs)

@
#EMBED Example/Effects/Todos.hs data Todos

#EMBED Example/Effects/Todos.hs loadAll
@

Once you've created an 'Effect', you add it to any 'HyperView' or 'Page' as a constraint.

From [Example.Todo](https://docs.hyperbole.live/todos):

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Todo.hs simplePage
@

When you create your 'Application', run any 'Effect's you need. Here we are using a runner that implements the effect with sessions from 'Hyperbole', but you could write a different runner that connects to a database instead.

From [example/Main.hs](https://github.com/seanhess/hyperbole/blob/latest/example/Main.hs):

@
app :: Application
app = do
  liveApp
    toDocument
    (runApp . routeRequest $ router)
 where
  runApp :: (Hyperbole :> es, IOE :> es) => Eff (Concurrent : Todos : es) a -> Eff es a
  runApp = runTodosSession . runConcurrent
@

Implementing a database runner for a custom 'Effect' is beyond the scope of this documentation, but see the following:

* [Effectful.Dynamic.Dispatch](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) - Introduction to Effects
* [NSO.Data.Datasets](https://github.com/DKISTDC/level2/blob/main/src/NSO/Data/Datasets.hs) - Production Data Effect with a database runner
* [Effectful.Rel8](https://github.com/DKISTDC/level2/blob/main/types/src/Effectful/Rel8.hs) - Effect for the [Rel8](https://hackage.haskell.org/package/rel8) Postgres Library
-}
