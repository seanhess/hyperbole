{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrency.Progress where

import App.Docs
import Control.Monad (when)
import Data.Text (pack)
import Effectful
import Example.Colors
import Example.Concurrency.Tasks
import Example.Effects.Debug
import Example.View.Inputs (progressBar)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.GenRandom

-----------------------------------------------------------
-- Overlapping Progress Bars
-----------------------------------------------------------

type PercentPerTick = Int

data Progress = Progress TaskId
  deriving (Generic, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView Progress es where
  data Action Progress
    = GoProgress PercentPerTick
    deriving (Generic, ViewAction)

  update (GoProgress progPerTick) = do
    _ <- tick 0
    pure $ viewProgress 100
   where
    tick current = do
      -- pretend we did some work
      -- this will not block other hyperviews from updating
      delay 50
      let total = current + progPerTick

      when (total < 100) $ do
        pushUpdate $ viewProgress total
        tick total

viewProgressLoad :: PercentPerTick -> View Progress ()
viewProgressLoad p = el @ onLoad (GoProgress p) 50 $ none

viewProgress :: Int -> View Progress ()
viewProgress prg
  | prg >= 100 = viewComplete
  | otherwise = viewUpdating
 where
  viewComplete = do
    row ~ bg Success . color White . pad 5 $ "Complete"

  viewUpdating = do
    let pct = fromIntegral prg / 100
    Progress taskId <- viewId
    progressBar pct $ do
      el ~ grow $ text $ "Task" <> pack (show taskId)

source :: ModuleSource
source = $(moduleSource)
