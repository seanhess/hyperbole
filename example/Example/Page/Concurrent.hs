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
    col (pad 20 . gap 10 . grow) $ do
      hyper (Progress 1) $ viewProgress 0
      hyper (Progress 2) $ viewProgress 0
      hyper (Progress 3) $ viewProgress 0
      hyper (Progress 4) $ viewProgress 0
      hyper (Progress 5) $ viewProgress 0

type TaskId = Int

data Progress = Progress TaskId
  deriving (Show, Read, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView Progress es where
  data Action Progress
    = CheckProgress Int
    deriving (Show, Read, ViewAction)
  update (CheckProgress prg) = do
    -- this will not block other hyperviews from updating
    dl <- genRandom @Int (0, 1000)
    delay dl

    -- pretend check update of a task
    nextProgress <- genRandom (0, 5)

    pure $ viewProgress (prg + nextProgress)

viewProgress :: Int -> View Progress ()
viewProgress prg
  | prg >= 100 = viewComplete
  | otherwise = viewUpdating prg

viewComplete :: View Progress ()
viewComplete = do
  row (bg Success . color White . pad 5) "Complete"

viewUpdating :: Int -> View Progress ()
viewUpdating prg = do
  let pct = fromIntegral prg / 100
  Progress taskId <- viewId
  col (onLoad (CheckProgress prg) 0) $ do
    progressBar pct $ do
      el grow $ text $ "Task" <> pack (show taskId)

progressBar :: Float -> View context () -> View context ()
progressBar pct content = do
  row (bg Light) $ do
    row (bg PrimaryLight . width (Pct pct) . pad 5) content
