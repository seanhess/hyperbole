{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Concurrent where

import Data.Text (pack)
import Effectful
import Example.AppRoute
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Random
import Example.View.Inputs (progressBar)
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Eff es (Page '[Progress])
page = do
  pure $ exampleLayout Concurrent $ do
    col ~ pad 20 . gap 10 . grow $ do
      hyper (Progress 1 100) $ viewProgress 0
      hyper (Progress 2 200) $ viewProgress 0
      hyper (Progress 3 300) $ viewProgress 0
      hyper (Progress 4 400) $ viewProgress 0
      hyper (Progress 5 500) $ viewProgress 0

type TaskId = Int

data Progress = Progress TaskId Milliseconds
  deriving (Generic, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView Progress es where
  data Action Progress
    = CheckProgress Int
    deriving (Generic, ViewAction)
  update (CheckProgress prg) = do
    Progress _ dly <- viewId

    -- this will not block other hyperviews from updating
    delay dly

    -- pretend check update of a task
    nextProgress <- genRandom (0, 5)

    pure $ viewProgress (prg + nextProgress)

viewProgress :: Int -> View Progress ()
viewProgress prg
  | prg >= 100 = viewComplete
  | otherwise = viewUpdating prg

viewComplete :: View Progress ()
viewComplete = do
  row ~ bg Success . color White . pad 5 $ "Complete"

viewUpdating :: Int -> View Progress ()
viewUpdating prg = do
  let pct = fromIntegral prg / 100
  Progress taskId _ <- viewId
  col @ onLoad (CheckProgress prg) 0 $ do
    progressBar pct $ do
      el ~ grow $ text $ "Task" <> pack (show taskId)
