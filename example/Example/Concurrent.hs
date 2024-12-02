{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrent where

import Data.Text (pack)
import Effectful
import Example.Effects.Debug
import Web.Hyperbole


page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Page es '[Contents]
page = do
  pure $ do
    col (pad 20) $ do
      hyper (Contents 50) $ viewPoll 1
      hyper (Contents 1000) $ viewPoll 100


data Contents = Contents Milliseconds
  deriving (Show, Read, ViewId)


instance HyperView Contents where
  data Action Contents
    = Load Int
    deriving (Show, Read, ViewAction)


instance (Debug :> es) => Handle Contents es where
  handle (Load n) = do
    Contents dl <- viewId
    -- BUG: this is blocking the thread, so the short poll has to wait for the long to finish before continuing
    delay dl
    pure $ viewPoll (n + 1)


viewPoll :: Int -> View Contents ()
viewPoll n = do
  onLoad (Load n) 0 $ do
    row (gap 10) $ do
      el_ "Polling:"
      text $ pack (show n)
