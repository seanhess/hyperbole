{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Push where

import Control.Monad (forM_)
import App.Docs
import Effectful
import Example.Colors
import Example.Effects.Debug
import Example.Style.Cyber (btn)
import Example.View.Inputs (progressBar)
import Web.Atomic.CSS
import Web.Hyperbole

data Tasks = Tasks
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView Tasks es where
  data Action Tasks
    = RunLongTask
    | Interrupt
    deriving (Generic, ViewAction)

  type Concurrency Tasks = Replace

  update RunLongTask = do
    forM_ [1 :: Float .. 100] $ \n -> do
      pushUpdate $ taskView (n / 100)
      delay 50
    pure $ taskView 1
  update Interrupt = do
    pure $ col ~ gap 10 $ do
      el "Interrupted!"
      taskView 0

taskView :: Float -> View Tasks ()
taskView pct = col ~ gap 10 $ do
  taskBar

  if isRunning
    then button Interrupt ~ btn $ "Interrupt"
    else button RunLongTask ~ btn . whenLoading disabled $ "Run Task"
 where
  taskBar
    | pct == 0 = el ~ bg Light . pad 5 $ "Task"
    | pct >= 1 = row ~ bg Success . color White . pad 5 $ el $ text "Complete"
    | otherwise = progressBar pct "Task"

  isRunning = pct > 0 && pct < 1

source :: ModuleSource
source = $(moduleSource)
