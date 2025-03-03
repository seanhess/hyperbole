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

    -- ** HTML Views
    -- $view-functions-intro

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

    -- * Functions, not Components #reusable#
    -- $reusable

    -- * Pages #pages#
    -- $practices-pages

    -- * Examples
    -- $examples

    -- * Application
    liveApp
  , Warp.run
  , basicDocument
  , Page
  , runPage

    -- ** Type-Safe Routes
  , routeRequest -- maybe belongs in an application section
  , Route (..)
  , routeUrl
  , route

    -- * Hyperbole Effect
  , Hyperbole

    -- ** Response
  , respondEarly
  , notFound
  , redirect

    -- ** Request
  , request
  , Request (..)

    -- ** Query Params #query#
  , query
  , setQuery
  , param
  , lookupParam
  , setParam
  , deleteParam
  , queryParams

    -- ** Sessions #sessions#
  , session
  , saveSession
  , lookupSession
  , modifySession
  , modifySession_
  , deleteSession
  , Session (..)

    -- * HyperView
  , HyperView (..)
  , hyper
  , HasViewId (..)

    -- * Interactive Elements
  , button
  , search
  , dropdown
  , option
  , Option

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

    -- * Type-Safe Forms

    -- | Painless forms with type-checked field names, and support for validation. See [Example.Forms](https://docs.hyperbole.live/formsimple)
  , FromForm (..)
  , FromFormF (..)
  , formData
  , GenFields (..)
  , fieldNames
  , FieldName (..)
  , FormFields
  -- , FormField (..)
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

    -- ** Validation
  , Validated (..)
  , isInvalid
  , validate
  , invalidText

    -- * Query Param Encoding
  , ToQuery (..)
  , FromQuery (..)
  , ToParam (..)
  , FromParam (..)
  , QueryData
  , DefaultParam (..)

    -- * Advanced
  , target
  , view
  , Response
  , ViewId
  , ViewAction
  , Root

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
import Web.Hyperbole.Data.QueryData
import Web.Hyperbole.Data.Form
import Web.Hyperbole.Data.Session
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Query
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response (notFound, redirect, respondEarly, view)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Effect.Session
import Web.Hyperbole.HyperView
import Web.Hyperbole.Page (Page, runPage)
import Web.Hyperbole.Route
import Web.Hyperbole.View
import Web.Hyperbole.View.Embed
import Web.Hyperbole.View.Form
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

They are divided into top-level 'Page's, which run side effects (such as loading data from a database), then respond with an HTML 'View'. The following application has a single 'Page' that displays a static "Hello World"

@
{\-# LANGUAGE DeriveAnyClass #-\}
{\-# LANGUAGE OverloadedStrings #-\}
{\-# LANGUAGE TypeFamilies #-\}
{\-# LANGUAGE DataKinds #-\}

module Main where

import Web.Hyperbole

#EMBED Example/Docs/BasicPage.hs main

#EMBED Example/Docs/BasicPage.hs page
@
-}


{- $interactive

We can embed one or more 'HyperView's to add type-safe interactivity to live subsections of our 'Page'. To start, first define a data type (a 'ViewId') that uniquely identifies that subsection of the page:

@
#EMBED Example/Docs/Interactive.hs data Message
@

Make our 'ViewId' an instance of 'HyperView' by:

* Create an 'Action' type with a constructor for every possible way that the user can interact with it
* Write an 'update' for each 'Action'

@
#EMBED Example/Docs/Interactive.hs instance HyperView Message
@

If an 'Action' occurs, the contents of our 'HyperView' will be replaced with 'update'.

To embed our new 'HyperView', add the 'ViewId' to the type-level list of 'Page', and then wrap the view in 'hyper'.

@
#EMBED Example/Docs/Interactive.hs page
@

Now let's add a button to trigger the 'Action'. Note that we must update the 'View'\'s 'context' to match our 'ViewId'. The compiler will tell us if we try to trigger actions that don't belong to our 'HyperView'

@
#EMBED Example/Docs/Interactive.hs messageView
@

If the user clicks the button, the contents of `hyper` will be replaced with the result of 'update', leaving the rest of the page untouched.
-}


{- $view-functions-intro

'View's are HTML fragments with embedded CSS

@
#EMBED Example/Docs/BasicPage.hs helloWorld
@

>>> Web.View.renderText $ el bold "Hello World"
<style type='text/css'>.bold { font-weight:bold }</style>
<div class='bold'>Hello World</div>

We can factor 'View's into reusable functions:

@
#EMBED Example/Docs/BasicPage.hs messageView

#EMBED Example/Docs/BasicPage.hs page'
@

We use plain functions to maintain a consistent look and feel rather than stylesheets:

@
header = bold
h1 = header . fontSize 32
h2 = header . fontSize 24
page = gap 10

example = col page $ do
  el h1 "My Page"
@

See [Web.View](https://hackage.haskell.org/package/web-view) for more details
-}


{- $view-functions

We showed above how we can factor 'View's into functions. It's best-practice to have a __main__ 'View' for each 'HyperView'. These take the form:

> state -> View viewId ()

There's nothing special about `state` or 'View' functions. They're just functions that take parameters and return a view.

We can write multiple view functions with our 'HyperView' as the 'context', and factor them however is most convenient:

@
#EMBED Example/Docs/ViewFunctions.hs messageButton
@

We can also create 'View' functions that work in any 'context':

@
#EMBED Example/Docs/ViewFunctions.hs header
@

Then we can refactor our main 'View' to use view functions to avoid repeating ourselves

@
#EMBED Example/Docs/ViewFunctions.hs messageView
@
-}


{- $practices

We've mentioned most of the Architecture of a hyperbole application, but let's go over each layer here:

* [Pages](#g:pages) - routes map to completely independent web pages
* [HyperViews](#g:hyperviews) - independently updating live subsections of a 'Page'
* Main [View Functions](#g:viewfunctions) - A view function that renders and updates a 'HyperView'
* [Reusable View Functions](#g:reusable) - Generic view functions you can use in any 'HyperView'
-}


{- $practices-multi

We can add as many 'HyperView's to a page as we want. Let's create another 'HyperView' for a simple counter

From [Example.Docs.MultiView](https://github.com/seanhess/hyperbole/blob/latest/examples/Example/Docs/MultiView.hs)

@
#EMBED Example/Docs/MultiView.hs data Count


#EMBED Example/Docs/MultiView.hs instance HyperView Count


#EMBED Example/Docs/MultiView.hs countView
@

We can use both 'Message' and 'Count' 'HyperView's in our page, and they will update independently:

@
#EMBED Example/Docs/MultiView.hs page
@
-}


{- $practices-same

We can embed multiple copies of the same 'HyperView' as long as the value of 'ViewId' is unique. Let's update `Message` to allow for more than one value:

See [Example.Page.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Docs/MultiCopies.hs data Message
@

Now we can embed multiple `Message` 'HyperView's into the same page. Each will update independently.

@
#EMBED Example/Docs/MultiCopies.hs page
@


This is especially useful if we put identifying information in our 'ViewId', such as a database id. The 'viewId' function gives us access to that info. See the [Contacts Example](https://docs.hyperbole.live/contacts) and the [Lazy Loading Example](https://docs.hyperbole.live/lazyloading)

From [Example.Page.LazyLoading](https://docs.hyperbole.live/lazyloading)

@
#EMBED Example/Page/LazyLoading.hs data LazyData

#EMBED Example/Page/LazyLoading.hs instance (Debug :> es, GenRandom :> es) => HyperView
@
-}


{- $practices-pages

An app has multiple 'Page's with different 'Route's that each map to a unique url path:

@
#EMBED Example/Docs/MultiPage.hs data AppRoute
@

When we define our app, we define a function that maps a 'Route' to a 'Page'

@
#EMBED Example/Docs/MultiPage.hs main
@

Each 'Page' is completely independent. The web page is freshly reloaded each time you switch routes. We can add type-safe links to other pages using 'route'

@
#EMBED Example/Docs/MultiPage.hs menu
@

If you need the same header or menu on all pages, use a view function:

@
#EMBED Example/Docs/MultiPage.hs exampleLayout

#EMBED Example/Docs/MultiPage.hs examplePage
@

As shown above, each 'Page' can contain multiple interactive 'HyperView's to add interactivity
-}


{- $practices-nested

We can nest smaller, specific 'HyperView's inside of a larger parent. You might need this technique to display a list of items which need to update themselves

Let's imagine we want to display a list of Todos. The user can mark individual todos complete, and have them update independently. The specific 'HyperView' might look like this:

From [Example.Docs.Nested](https://github.com/seanhess/hyperbole/blob/latest/examples/Example/Docs/Nested.hs)

@
#EMBED Example/Docs/Nested.hs data TodoItem

#EMBED Example/Docs/Nested.hs instance HyperView TodoItem
@

But we also want the entire list to refresh when a user adds a new todo. We need to create a parent 'HyperView' for the whole list.

List all allowed nested views by adding them to 'Require'

@
#EMBED Example/Docs/Nested.hs data AllTodos

#EMBED Example/Docs/Nested.hs instance HyperView AllTodos
@

Then we can embed the child 'HyperView' into the parent with 'hyper'

@
#EMBED Example/Docs/Nested.hs todosView
@
See this technique used in the [TodoMVC Example](https://docs.hyperbole.live/todos)
-}


{- $reusable

You may be tempted to use 'HyperView's to create reusable \"Components\". This leads to object-oriented designs that don't compose well. We are using a functional language, so our main unit of reuse should be functions!

We showed earlier that we can write a [View Function](#g:view-functions) with a generic 'context' that we can reuse in any view.  A function like this might help us reuse styles:

@
#EMBED Example/Docs/ViewFunctions.hs header
@

What if we want to reuse functionality too? We can pass an 'Action' into the view function as a parameter:

@
#EMBED Example/Docs/Component.hs styledButton
@

We can create more complex view functions by passing state in as a parameter. Here's a button that toggles between a checked and unchecked state:

@
#EMBED Example/View/Inputs.hs toggleCheckBtn
@

View functions can wrap other Views:

@
#EMBED Example/View/Inputs.hs progressBar
@


Don't leverage 'HyperView's for code reuse. Think about which subsections of a page ought to update independently. Those are 'HyperView's. If you need reusable functionality, use [view functions](#g:viewfunctions) instead.

* See [Example.View.DataTable](https://docs.hyperbole.live/datatable) for a more complex example
-}


{- $examples
https://docs.hyperbole.live is full of live examples demonstrating different features. Each example includes a link to the source code. Some highlights:

* [Simple](https://docs.hyperbole.live/simple)
* [Counter](https://docs.hyperbole.live/counter)
* [CSS Transitions](https://docs.hyperbole.live/transitions)
* [Lazy Loading](https://docs.hyperbole.live/lazyloading)
* [Forms](https://docs.hyperbole.live/formsimple)
* [Data Table](https://docs.hyperbole.live/datatable)
* [Sessions](https://docs.hyperbole.live/sessions)
* [Filter Items](https://docs.hyperbole.live/filter)
* [Autocomplete](https://docs.hyperbole.live/autocomplete)
* [Todo MVC](https://docs.hyperbole.live/todos)

The [National Solar Observatory](https://nso.edu) uses Hyperbole for the Level 2 Data creation tool for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/). It is completely [open source](https://github.com/DKISTDC/level2/). This production application contains complex interfaces, workers, databases, and more.
-}


{- $state-parameters

'HyperView's are stateless. They 'update' based entirely on the 'Action'. However, we can track simple state by passing it back and forth between the 'Action' and the 'View'

From [Example.Page.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Docs/State.hs instance HyperView Message

#EMBED Example/Docs/State.hs messageView
@
-}


{- $state-effects

For any real application with more complex state and data persistence, we need side effects.

Hyperbole relies on [Effectful](https://hackage.haskell.org/package/effectful) to compose side effects. We can use effects in a page or an 'update'. The 'Hyperbole' effect gives us access to the 'request' and 'Client' state, including 'session's and the 'query' 'param's. In this example the page keeps the message in the 'query' 'param's

From [Example.Docs.SideEffects](https://github.com/seanhess/hyperbole/blob/latest/examples/Example/Docs/SideEffects.hs)

@
#EMBED Example/Docs/SideEffects.hs page

#EMBED Example/Docs/SideEffects.hs instance HyperView Message
@


To use an 'Effect' other than 'Hyperbole', add it as a constraint to the 'Page' and any 'HyperView' instances that need it.

From [Example.Page.Counter](https://docs.hyperbole.live/counter)

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Page/Counter.hs instance (Reader
@

Then run the effect in your application

@
#EMBED Example/Page/Counter.hs app
@

* Read more about [Effectful](https://hackage.haskell.org/package/effectful)
-}


{- $state-databases

A database is no different from any other 'Effect'. We recommend you create a custom effect to describe high-level data operations.

From [Example.Effects.Todos](https://github.com/seanhess/hyperbole/blob/latest/examples/Example/Effects/Todos.hs)

@
#EMBED Example/Effects/Todos.hs data Todos

#EMBED Example/Effects/Todos.hs loadAll
@

Once you've created an 'Effect', you add it to any 'HyperView' or 'Page' as a constraint.

From [Example.Page.Todo](https://docs.hyperbole.live/todos):

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Page/Todo.hs simplePage
@

We run a custom effect in our Application just like any other. Here we implementing our custom effect using 'Hyperbole' 'sessions', but you could write a different runner that connects to a database instead.

@
#EMBED Example/Page/Todo.hs main
@

See [example/Main](https://github.com/seanhess/hyperbole/blob/0.4/example/Main.hs) for a full example application with multiple effects

Implementing a database runner for a custom 'Effect' is beyond the scope of this documentation, but see the following:

* [Effectful.Dynamic.Dispatch](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) - Introduction to Effects
* [NSO.Data.Datasets](https://github.com/DKISTDC/level2/blob/main/src/NSO/Data/Datasets.hs) - Production Data Effect with a database runner
* [Effectful.Rel8](https://github.com/DKISTDC/level2/blob/main/types/src/Effectful/Rel8.hs) - Effect for the [Rel8](https://hackage.haskell.org/package/rel8) Postgres Library
-}
