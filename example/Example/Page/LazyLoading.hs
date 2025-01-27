{-# LANGUAGE UndecidableInstances #-}

module Example.Page.LazyLoading where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Style
import Example.Effects.Debug
import Example.Effects.Random
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[SimplePolling, LazyData])
page = do
  pure $ exampleLayout LazyLoading $ do
    col (gap 20 . pad 20) $ do
      el (bold . fontSize 24) "Simple Polling"
      col (pad 15 . border 1) $ do
        hyper SimplePolling viewInit

      el (bold . fontSize 24) "Lazy Loading Items"
      col (gap 10) $ do
        forM_ pretendTasks $ \taskId -> do
          hyper (LazyData taskId) viewTaskLoad

-----------------------------------------------------------
-- Simple Polling
-----------------------------------------------------------

data SimplePolling = SimplePolling
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView SimplePolling es where
  data Action SimplePolling
    = Reload Int
    | Stop
    | Pause Int
    deriving (Show, Read, ViewAction)

  -- to stop, return a view without an onLoad
  update (Pause n) = do
    pure $ viewPaused n
  update Stop = do
    pure viewStopped
  update (Reload n) = do
    pure $ viewPoll n

viewInit :: View SimplePolling ()
viewInit = do
  button (Reload 1) btn "Start Polling"

viewStopped :: View SimplePolling ()
viewStopped = do
  button (Reload 1) btn "Restart Polling"

viewPaused :: Int -> View SimplePolling ()
viewPaused n = do
  col (gap 10) $ do
    button (Reload n) btn "Resume"
    viewStatus n

viewPoll :: Int -> View SimplePolling ()
viewPoll n = do
  col ((onLoad (Reload (n + 1)) 1000) . gap 10) $ do
    row (gap 5) $ do
      button (Pause n) btn "Pause"
      button Stop btn "Stop"
    viewStatus n


viewStatus :: Int -> View SimplePolling ()
viewStatus n = do
  el id $ do
    text "Polling... "
    text $ pack $ show n

-----------------------------------------------------------
-- Lazy Loading Expensive Data
-----------------------------------------------------------

type TaskId = Text

data Task = Task
  { taskId :: TaskId
  , details :: Text
  }

pretendLoadTask :: (Debug :> es, GenRandom :> es) => TaskId -> Eff es Task
pretendLoadTask taskId = do
  -- pretend it takes time to load
  randomDelay <- genRandom (100, 1000)
  delay randomDelay

  pure $ Task taskId $ "Details for " <> taskId

pretendTasks :: [TaskId]
pretendTasks = fmap (pack . show @Int) [0 .. 100]

data LazyData = LazyData TaskId
  deriving (Show, Read, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
  data Action LazyData
    = Details
    deriving (Show, Read, ViewAction)

  update Details = do
    LazyData taskId <- viewId
    task <- pretendLoadTask taskId
    pure $ viewTaskDetails task

viewTaskLoad :: View LazyData ()
viewTaskLoad = do
  -- 100ms after rendering, get the details
  el (onLoad Details 100 . color Secondary) $ do
    text "..."

viewTaskDetails :: Task -> View LazyData ()
viewTaskDetails task = do
  row (color Success) $ do
    text task.details
