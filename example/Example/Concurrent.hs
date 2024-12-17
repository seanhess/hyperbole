{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrent where

import Data.Text (pack)
import Effectful
import Example.Effects.Debug
import Web.Hyperbole


page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Page es '[Poller]
page = do
  pure $ do
    col (pad 20) $ do
      hyper (Poller 50) $ viewPoll 1
      hyper (Poller 1000) $ viewPoll 100


data Poller = Poller Milliseconds
  deriving (Show, Read, ViewId)


instance (Debug :> es) => HyperView Poller es where
  data Action Poller
    = Load Int
    deriving (Show, Read, ViewAction)
  update (Load n) = do
    Poller dl <- viewId
    -- BUG: this is blocking the thread, so the short poll has to wait for the long to finish before continuing
    delay dl
    pure $ viewPoll (n + 1)


viewPoll :: Int -> View Poller ()
viewPoll n = do
  row (gap 10 . onLoad (Load n) 0) $ do
    el_ "Polling:"
    text $ pack (show n)
