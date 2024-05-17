{- |
Module:      Web.Hyperbole
Copyright:   (c) 2024 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Create fully interactive HTML applications using Haskell. Type-safe routing, forms, Inspired by HTMX
-}
module Web.Hyperbole
  ( module Web.Hyperbole.Effect
  , module Web.Hyperbole.HyperView

    -- * Forms
  , module Web.Hyperbole.Forms

    -- * Type-Safe Routes
  , Route
  , routeUrl
  , route

    -- * Create an Application
  , liveApp
  , routeRequest -- maybe belongs in an application section
  , Warp.run
  , Application
  , basicDocument

    -- * Re-exported methods from Web.view
  , module Web.View

    -- * Effects
  , Eff
  , (:>)
  , Effect.request
  , Effect.session

    -- * Embedded Javascript and CSS
  , module Web.Hyperbole.Embed
  ) where

import Effectful
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Web.Hyperbole.Application
import Web.Hyperbole.Effect (Hyperbole, Page, Request (..), Response, Server, clearSession, formData, getEvent, hyper, load, lookupEvent, lookupParam, notFound, page, parseError, redirect, reqParam, reqParams, reqPath, respondEarly, routeRequest, runHyperbole, setSession, view)
import Web.Hyperbole.Effect qualified as Effect
import Web.Hyperbole.Embed
import Web.Hyperbole.Forms
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.View hiding (button, form, input, label)

