{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrency.LazyLoading where

import App.Docs
import Effectful
import Example.Colors
import Example.Concurrency.Tasks
import Example.Effects.Debug
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.GenRandom

-----------------------------------------------------------
-- Lazy Loading Expensive Data
-----------------------------------------------------------

data LazyData = LazyData TaskId
  deriving (Generic, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
  data Action LazyData
    = Details
    deriving (Generic, ViewAction)

  update Details = do
    LazyData taskId <- viewId
    task <- pretendLoadTask taskId
    pure $ viewTaskDetails task

viewTaskLoad :: View LazyData ()
viewTaskLoad = do
  -- 100ms after rendering, get the details
  el @ onLoad Details 100 ~ bg GrayLight . textAlign AlignCenter $ do
    text "..."

viewTaskDetails :: Task -> View LazyData ()
viewTaskDetails task = do
  el ~ color Success . textAlign AlignCenter $ do
    text task.details

source :: ModuleSource
source = $(moduleSource)
