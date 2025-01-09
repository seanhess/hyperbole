{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Concurrent where

import Data.Text (pack)
import Effectful
import Example.AppRoute
import Example.Effects.Debug
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Eff es (Page '[Poller])
page = do
  pure $ exampleLayout Concurrent $ do
    col (pad 20) $ do
      hyper (Poller 250) $ viewPoll 1
      hyper (Poller 1000) $ viewPoll 100

data Poller = Poller Milliseconds
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView Poller es where
  data Action Poller
    = Load Int
    deriving (Show, Read, ViewAction)
  update (Load n) = do
    Poller dl <- viewId
    delay dl
    pure $ viewPoll (n + 1)

viewPoll :: Int -> View Poller ()
viewPoll n = do
  row (gap 10 . onLoad (Load n) 0) $ do
    el_ "Polling:"
    text $ pack (show n)
