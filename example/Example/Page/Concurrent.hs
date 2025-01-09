{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Concurrent where

import Data.Text (pack)
import Effectful
import Example.AppRoute
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Random
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Eff es (Page '[Progress])
page = do
  pure $ exampleLayout Concurrent $ do
    col (pad 20) $ do
      hyper (Progress 1) $ viewProgress 1
      hyper (Progress 2) $ viewProgress 100

type TaskId = Int

data Progress = Progress TaskId
  deriving (Show, Read, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView Progress es where
  data Action Progress
    = Load Int
    deriving (Show, Read, ViewAction)
  update (Load n) = do
    Progress dl <- viewId
    -- BUG: this is blocking the thread, so the short poll has to wait for the long to finish before continuing
    dl <- randomDelayMs
    delay dl
    pure $ viewProgress (n + 1)
   where
    randomDelayMs = genRandom @Int (0, 1000)

viewProgress :: Int -> View Progress ()
viewProgress n = do
  row (gap 10 . onLoad (Load n) 0 . bg Secondary) $ do
    text $ pack (show n)
