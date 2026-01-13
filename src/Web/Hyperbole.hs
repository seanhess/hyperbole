{- |
Module:      Web.Hyperbole
Copyright:   (c) 2024 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Create fully interactive HTML applications with type-safe serverside Haskell. Inspired by [HTMX](https://htmx.org/), [Elm](https://elm-lang.org/), and [Phoenix LiveView](https://www.phoenixframework.org/)

* [hyperbole.live](https://hyperbole.live) - documentation and examples
* [github](https://github.com/seanhess/hyperbole) - issues and source code
-}
module Web.Hyperbole
  ( -- * Application #application#
    liveApp
  , Warp.run

    -- ** Page
  , Page
  , runPage

    -- ** Document
  , document
  , quickStartDocument
  , DocumentHead
  , quickStart
  , mobileFriendly

    -- ** Type-Safe Routes #routes#
  , Route (..)
  , routeRequest -- maybe belongs in an application section
  , routeUri
  , route

    -- * Hyperbole Effect #hyperbole-effect#
  , Hyperbole

    -- ** Request #request#
  , request
  , Request (..)

    -- ** Response #response#
  , respondError
  , respondErrorView
  , notFound
  , redirect

    -- ** Query #query#
    -- $query
  , ToQuery (..)
  , FromQuery (..)
  , query
  , setQuery
  , modifyQuery
  , clearQuery
  , param
  , lookupParam
  , setParam
  , deleteParam
  , queryParams

    -- ** Sessions #sessions#
    -- $sessions
  , Session (..)
  , session
  , saveSession
  , lookupSession
  , modifySession
  , modifySession_
  , deleteSession

    -- ** Control Client #client#
  , pageTitle
  , trigger
  , pushEvent
  , pushUpdate

    -- * HyperView #hyperview#
  , HyperView (..)
  , hyper
  , hyperState
  , HasViewId (..)

    -- * Interactive Elements #interactive#
  , button
  , search
  , dropdown
  , option
  , Option

    -- * Events
  , onClick
  , onDblClick
  , onMouseEnter
  , onMouseLeave
  , onInput
  , onLoad
  , DelayMs
  , onKeyDown
  , onKeyUp
  , Key (..)

    -- * Type-Safe Forms #forms#
    -- $forms
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
  , checkbox
  , radioGroup
  , radio
  , select
  , checked
  , textarea
  , submit
  , View.placeholder
  , InputType (..)

    -- ** Validation
  , Validated (..)
  , isInvalid
  , validate
  , invalidText

    -- * Query Param Encoding #query-param#
  , QueryData
  , ToParam (..)
  , FromParam (..)
  , ToEncoded
  , FromEncoded

    -- * Advanced #advanced#
  , target
  , Response
  , Root
  , ConcurrencyMode (..)

    -- * Exports #exports#

    -- ** View
  , View (..)
  , module View

    -- ** Embeds

    -- | Embedded CSS and Javascript to include in your document function. See 'quickStartDocument'
  , module Web.Hyperbole.View.Embed

    -- ** Effectful
    -- $effects
  , module Effectful

    -- ** Other
  , URI (..)
  , uri
  , Application
  , module GHC.Generics
  , Default (..)
  , ToJSON
  , FromJSON
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Effectful (Eff, (:>))
import GHC.Generics (Generic, Rep)
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Web.Atomic.CSS ()
import Web.Atomic.Types ()
import Web.Hyperbole.Application
import Web.Hyperbole.Data.Encoded (FromEncoded, ToEncoded)
import Web.Hyperbole.Data.Param
import Web.Hyperbole.Data.QueryData
import Web.Hyperbole.Document
import Web.Hyperbole.Effect.Client
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Query
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response
import Web.Hyperbole.Effect.Session
import Web.Hyperbole.HyperView
import Web.Hyperbole.HyperView.Forms
import Web.Hyperbole.Page (Page, runPage)
import Web.Hyperbole.Route
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View hiding (placeholder)
import Web.Hyperbole.View qualified as View hiding (Attributable, Attributes, View)
import Web.Hyperbole.View.Embed


{- $documentation

Please visit https://hyperbole.live for documentation and examples
-}

-- TODO: NSO link
