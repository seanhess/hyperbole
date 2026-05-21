{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrency.Polling where

import App.Docs
import Data.Text (pack)
import Effectful
import Example.Effects.Debug
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

-----------------------------------------------------------
-- Simple Polling
-----------------------------------------------------------

data Poller = Poller
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView Poller es where
  data Action Poller
    = Reload Int
    | Stop
    | Pause Int
    deriving (Generic, ViewAction)

  -- to stop, return a view without an onLoad
  update (Pause n) = do
    pure $ viewPaused n
  update Stop = do
    pure viewStopped
  update (Reload n) = do
    pure $ viewPoll n

viewInit :: View Poller ()
viewInit = do
  row $ do
    button (Reload 1) "Start Polling" ~ btn

viewStopped :: View Poller ()
viewStopped = do
  row $ do
    button (Reload 1) "Restart Polling" ~ btn

viewPaused :: Int -> View Poller ()
viewPaused n = do
  col ~ gap 10 $ do
    row $ do
      button (Reload n) "Resume" ~ btn
    viewStatus n

viewPoll :: Int -> View Poller ()
viewPoll n = do
  -- reload every 200ms + round trip delay
  col @ onLoad (Reload (n + 1)) 250 ~ gap 10 $ do
    row ~ gap 5 $ do
      button (Pause n) "Pause" ~ btn
      button Stop "Stop" ~ btn
    viewStatus n

viewStatus :: Int -> View Poller ()
viewStatus n = do
  el $ do
    text "Polling... "
    text $ pack $ show n

source :: ModuleSource
source = $(moduleSource)
