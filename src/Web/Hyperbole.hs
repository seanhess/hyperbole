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

    -- * Getting Started

    -- ** Building a page with Hyperbole in a nutshell - `Page`s and `View`s
    -- $hello

    -- ** Updating Parts of your Page independently - `HyperView`s #hyperviews#
    -- $interactive

    -- ** Let a `HyperView` carry a Simple State - Without Effects
    -- $state-simple
    
    -- ** Reusing `View` Functions with `Action`s across different `HyperView`s #reusable#
    -- $reusable

    -- * Key Concepts
    -- ** Entities
    -- $entities

    -- ** When to use what?
    -- $when-what

    -- ** Main Flow
    -- $main-flow

    -- ** Anti-Patterns
    -- $anti-patterns
    
    -- * Advanced

    -- ** Updating Parts of `HyperView`s independently (`HyperView`s within `HyperView`s) - `Require`
    -- $practices-nested

    -- ** Side Effects - State across `HyperView`s, Persistence / DB and IO
    -- *** Introduction
    -- $state-effects

    -- *** Using builtin Session State
    -- $state-effects-hyperbole

    -- *** Using non-builtin State
    -- $state-effects-existing

    -- *** Databases and Custom Effects
    -- $state-custom

    -- ** Pages #pages#
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
  , ToQueryData (..)
  , FromQueryData (..)

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

    -- | Painless forms with type-checked field names, and support for validation. See [Example.Forms](https://docs.hyperbole.live/formsimple)
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
import Web.Hyperbole.Effect.QueryData (FromQueryData (..), ToQueryData (..))
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

Single Page Applications (SPAs) require the programmer to write two programs: a Javascript client and a Server, which both must conform to a common API.

Hyperbole allows us to instead write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in different ways, including [HTMX](https://htmx.org/), [Phoenix LiveView](https://www.phoenixframework.org/), and others.

Hyperbole has the following advantages:

* 100% Haskell
* Type safe views, actions, routes, and forms
* Elegant interface with little boilerplate
* VirtualDOM updates over sockets, fallback to HTTP
* Easy to use

While Hyperbole

* extends capability of UI elements (like [HTMX](https://htmx.org/)) /it uses/ Haskell's type-system to prevent common errors and provide default functionality. Specifically, a page has multiple update targets called `HyperView`s. These are automatically targeted by any UI element that triggers an action inside them. The compiler makes sure that actions and targets match
* upgrades the `Page` to a fast WebSocket connection (like [Phoenix LiveView](https://www.phoenixframework.org/)) /it uses/ VirtualDOM for live updates.
* uses an `update` function to process `Action`s (like [Elm](https://elm-lang.org/)) /it greatly simplifies/ the Elm Architecture by remaining stateless. Effects are handled by [Effectful](https://hackage.haskell.org/package/effectful). `form`s are easy to use with minimal boilerplate.

Hyperbole depends heavily on the following frameworks

* [Effectful](https://hackage.haskell.org/package/effectful-core)
* [Web View](https://hackage.haskell.org/package/web-view)
-}


{- $hello

You define a `Page` by combining those UI elements that should be loaded on initial load. You tell [Warp](https://hackage.haskell.org/package/warp) to serve that `Page`.
Start by using UI elements directly.

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


Then factor them out into `View`s. You may use multiple `View`s to make up a `Page`.

@

#EMBED Example/Intro/BasicPage.hs main'

#EMBED Example/Intro/BasicPage.hs messagePage'

#EMBED Example/Intro/BasicPage.hs messageView1

@

You may pass a `View` function also arguments like:

@

#EMBED Example/Intro/BasicPage.hs messageView2

@

-}

{- $interactive

Whatever part of your page should update independently (after initial page load), can do so by becoming a `HyperView`. A `View` becomes a `HyperView` by making it an instance of the `HyperView` typeclass. To do so, you build an explicit `View` type by making it an instance of `ViewId`:

@
#EMBED Example/Intro/Interactive.hs data Message
@

You then make the `ViewId` `Message` an instance of `HyperView` typeclass by:

* Defining the (ADT of) different `Action`s you want to differentiate and
* defining in the `update` function the way the `View` should be rebuilt (which simply calls a function that returns a `View Message ()` in our case).

@
#EMBED Example/Intro/Interactive.hs instance HyperView Message
@

Wrap a @hyper Message@ around the @messageView "Hello"@ function.
/This makes sure that any `Action` from the `ViewId` is being handled by the `update` function defined for `Message`./

@
#EMBED Example/Intro/Interactive.hs messagePage
@

In the `ViewId` `Message` then, you a `button` to trigger the `Action`:

@
#EMBED Example/Intro/Interactive.hs messageView
@

The contents of `hyper` will be replaced with the result of `update`, leaving the rest of the page untouched.
-}

{- $state-simple

So far, any `HyperView` was stateless as they were entirely based on `Action`.
Once we extend the `Action` by a value, `update` can forward it to the `View` generating function,
to realise a `View` using the carried forward state, like our counter value.
So, how would you build a simple counter or anything that uses the previous state?

Let the `Action` carry the state (within the `View`), once triggered, propagate it through the `update` function to our @messageView@ function, where the updated value is being used as new state, and so forth:

From [Example.Page.Simple](https://docs.hyperbole.live/simple):

@
#EMBED Example/Page/Simple.hs instance HyperView Message
@

And as @messageView@ takes in the previous state, you use it at a carry-on for the action @Louder m@ (@m@ is the state). This way whenever the action `Action` gets triggered, it uses the previous state.

@
#EMBED Example/Page/Simple.hs messageView
@

-}


{- $reusable

As it is simple to reuse components without `Action`s on your page (by creating functions that return generic `View`s),

@
#EMBED Example/Intro/ViewFunctions.hs header
@

you may want to reuse components with `Action`s across different `HyperView`s.

That means your `Action`s need to be different to match different `HyperView`s.
You can reuse components containing an action across different `HyperView`s by writing `View` functions that take in the `Action`.
That way you can feed them in-place the `Action` you want that matches the `HyperView` you are using the component in-place.

@
#EMBED Example/Intro/Component.hs styledButton
@

We can create more complex view functions by passing state in as a parameter. Here's a button that toggles between a checked and unchecked state:

@
#EMBED Example/View/Inputs.hs toggleCheckBtn
@

Don't leverage 'HyperView's for code reuse. Think about which subsections of a page ought to update independently. Those are 'HyperView's. If you need reusable functionality, use view functions instead.

* See [Example.View.DataTable](https://docs.hyperbole.live/datatable) for a more complex example
-}

{- $entities

[@Page@]: Defines what data is loaded on initial page load (outermost monad), then defining what is visually on the page (innermost monad); composed of one or more `View`s (that need to match those listed in the `Page`s type signature) of which any might be `HyperView`s (then they need to be wrapped with `hyper`).
[@View@]: A static construct of UI elements not taking any arguments.
[@View Function@]: without Actions: Taking (parameters for) design elements as input to return a View with a generic context. Therefore reusable across different `Pages`.
[@View Function with Actions@]: Taking an `Action` as input to return a `View` with a generic context. Therefore reusable across different pages. KEY to realise architecture that composes well (instead of misusing `HyperView`s to facilitate code-reuse when parts are actually not required to update independently).
[@HyperView@]: A part of your page that is being able to update independently after initial `Page` load. Extends any `View` (by ADT of) `Action`s and how on which `Action` the content should be updated and replaced. Allows also sub parts of a `HyperView` to be independently updated from the parent `HyperView` by specifying the child `HyperView` in the `Require` of the parent `HyperView`.

-}

{- $when-what

[@Which parts should update independently?@] Those (and only those) should be `HyperView`s, including nested ones.
[@Which style components are used across different `View`s or `Pages`?@] Create generic `View` functions and use them in any place.
[@Which components with `Action`s are reused across different `HyperView`s?@} Create generic `View` functions that take the `Action` as an argument and use them in any place by feeding it the correct `Action`.
[@Where do I want to load/save the data?@] If loading data at start of server is just fine, you don’t need effects. If you want to do it while the server is running (like DB ops, session management, global state, etc.), define functions that emit effects, constrain the `Page` with that effect type and wrap it before serving the `Page` within the handler function that you need to define to handle each `Effect`.

-}

{- $main-flow

  1. Load the data - either on page load, or in response to an action.
  2. Write a main View Function that takes that data as a parameter.
  3. Write as many other helper view functions as you need to factor your code nicely.

-}

{- $anti-patterns

[@(Ab)using `HyperView`s to increase code-reuse@]: If you don't need views to update (after page load), you don't need hyper. If you still use it, you risk creating a bad architecture. Use View Functions with Actions as arguments instead.

-}

{- $practices-nested
What if you want to `update` only a part of what already is a `HyperView`? You simply call another `HyperView` in the parent `HyperView`.
To make it work, you need to tell the parent `HyperView` in its instance definition to `Require` the child `HyperView`.

From [Example.Intro.Nested](https://github.com/seanhess/hyperbole/blob/latest/example/Example/Intro/Nested.hs)

@
#EMBED Example/Intro/Nested.hs instance HyperView TodoItem
@

And here is how you use the `HyperView` inside the parent `HyperView`:
@
#EMBED Example/Intro/Nested.hs todosView
@

You are now able to update one child element and the parent (with all children) independently (after initial page load).

See this technique used in the [TodoMVC Example](https://docs.hyperbole.live/todos).
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

See [Example.Page.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Page/Simple.hs data Message
@

Now we can embed multiple `Message` 'HyperView's into the same page. Each will update independently.

@
#EMBED Example/Page/Simple.hs messagePage
@


This is especially useful if we put identifying information in our 'ViewId', such as a database id. The [Contacts Example](https://docs.hyperbole.live/contacts) uses this to allow the user to edit multiple contacts on the same page. The 'viewId' function gives us access to that info

From [Example.Contacts](https://docs.hyperbole.live/contacts)

@
#EMBED Example/Page/Contact.hs data Contact

#EMBED Example/Page/Contact.hs instance (Users :> es,
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

If you need the same header or menu on all pages, use a view function:

@
#EMBED Example/Intro/MultiPage.hs exampleLayout

#EMBED Example/Intro/MultiPage.hs examplePage
@

As shown above, each 'Page' can contain multiple interactive 'HyperView's to add interactivity
-}

{- $examples
https://docs.hyperbole.live is full of live examples demonstrating different features. Each example includes a link to the source code. Some highlights:

* [Simple](https://docs.hyperbole.live/simple)
* [Counter](https://docs.hyperbole.live/counter)
* [CSS Transitions](https://docs.hyperbole.live/transitions)
* [Lazy Loading](https://docs.hyperbole.live/lazyloading)
* [Forms](https://docs.hyperbole.live/formsimple)
* [Data Table](https://docs.hyperbole.live/datatable)
* [Filter Items](https://docs.hyperbole.live/filter)
* [Autocomplete](https://docs.hyperbole.live/livesearch)
* [Todo MVC](https://docs.hyperbole.live/todos)

The [National Solar Observatory](https://nso.edu) uses Hyperbole for the Level 2 Data creation tool for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/). It is completely [open source](https://github.com/DKISTDC/level2/). This production application contains complex interfaces, workers, databases, and more.
-}


{- $state-parameters

@
#EMBED Example/Intro/State.hs instance HyperView Message

#EMBED Example/Intro/State.hs messageView
@
-}


{- $state-effects

At times you want to use a state across `HyperView`s, want database operations, file system access or any other IO operation while a `HyperView` updates independently.

The concept in Hyperbole is straightforward:

1. You use or write functions that emit side-effects.
2. You add the `Effect`'s type as a constraint to the `Page` that might emit them.
3. You wrap the `Page` before being served with a handler that catches and handles each different side-effect.

Any side-effect is forwarded through the context of a `Page` (you may think of `HyperView` as well as `Page` allowing only specific side-effects to leak through, reaching the handler function, doing its work and returning the value back to where the side-effect originated to be used over there).
(While the `update` function for a type instance definition for `HyperView` is a handler for the defined `Action`s /within/ the context of a `HyperView`, similarly the handler function for a specific `Effect` type handles the different variants of `Effect`s /but/ crossing the `HyperView` and `Page` boundary and returning.)

Hyperbole relies on [Effectful](https://hackage.haskell.org/package/effectful) to compose side effects. We can use effects in a `Page` or an `update`. In this example, using the builtin session state, each client stores the latest message in their session.

-}

{- $state-effects-hyperbole

From [Example.Page.Simple](https://docs.hyperbole.live/simple)

@
#EMBED Example/Intro/SideEffects.hs messagePage

#EMBED Example/Intro/SideEffects.hs instance HyperView Message
@
-}

{- $state-effects-existing

To use an `Effect` other than `Hyperbole`, add it as a constraint to the `Page` and any `HyperView` instances that need it. Then run the effect in your application.

From [Example.Page.Counter](https://docs.hyperbole.live/counter)

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Page/Counter.hs instance (Reader
@
-}

{- $state-custom

How to create and use custom effects:

* Define your own `Effect` with its variants.
* For each variant, create a function that emits itself and return the value (we want to use in a `HyperView`).
* Create a handler function that takes all `Effect` variants and processes the side-effects to realise its return value (that becomes available in the `HyperView`).
* Add the (custom) `Effect` type as constraint to the `HyperView` and `Page` (to let the `Effect` leak through).
* Wrap the `Page` within the handler function to catch and handle any `Effect` that has leaked through `HyperView` and `Page`.
A database is no different from any other `Effect`. Create a custom effect to describe high-level data operations.


When there is no existing `Effect` you can use, you can hand roll one for your use by fleshing out the different `Effect`s variants:

From [Example.Effects.Todos](https://github.com/seanhess/hyperbole/blob/latest/example/Example/Effects/Todos.hs):

@
#EMBED Example/Effects/Todos.hs data Todos
@

Proceed with defining the function that handles the case, for example for @LoadAll@:

@
#EMBED Example/Effects/Todos.hs loadAll
@
(We want to call this function inside a `HyperView` to emit the side-effect and return the value to be used in the `HyperView`.)

Then define a handler function (similar to `update`) that takes in an `Effect` and sends it to the right function:

@
#EMBED Example/Effects/Todos.hs runTodosSession
@

Once you've created an `Effect`, add it to any `HyperView` or `Page` as a constraint. This is @(Todos :> es) =>@ in our case.

From [Example.Page.Todo](https://docs.hyperbole.live/todos):

@
{\-# LANGUAGE UndecidableInstances #-\}

#EMBED Example/Page/Todo.hs simplePage
@

When you create your `Application`, run any `Effect`s you need. Here we are using a runner that implements the effect with sessions from `Hyperbole`, but you could write a different runner that connects to a database instead.

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

Implementing a database runner for a custom `Effect` is beyond the scope of this documentation, but see the following:

* [Effectful.Dynamic.Dispatch](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) - Introduction to Effects
* [NSO.Data.Datasets](https://github.com/DKISTDC/level2/blob/main/src/NSO/Data/Datasets.hs) - Production Data Effect with a database runner
* [Effectful.Rel8](https://github.com/DKISTDC/level2/blob/main/types/src/Effectful/Rel8.hs) - Effect for the [Rel8](https://hackage.haskell.org/package/rel8) Postgres Library
-}
