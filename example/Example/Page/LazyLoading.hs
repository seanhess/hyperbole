{-# LANGUAGE UndecidableInstances #-}

module Example.Page.LazyLoading where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text, pack)
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Random
import Example.Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)

page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Polling, LazyData])
page = do
  pure $ exampleLayout LazyLoading $ do
    col (gap 20 . pad 20) $ do
      el (bold . fontSize 24) "Polling"
      col (pad 15 . border 1) $ do
        hyper Polling viewInit

      el (bold . fontSize 24) "Lazy Loading Items"
      row (gap 10 . fwrap) $ do
        forM_ pretendTasks $ \taskId -> do
          el (border 1 . width 120 . pad 5) $ do
            hyper (LazyData taskId) viewTaskLoad

-----------------------------------------------------------
-- Simple Polling
-----------------------------------------------------------

data Polling = Polling
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView Polling es where
  data Action Polling
    = Reload Int
    | Stop
    | Pause Int
    deriving (Show, Read, ViewAction)

  -- to stop, return a view without an onLoad
  update (Pause n) = do
    delay 1000
    pure $ viewPaused n
  update Stop = do
    pure viewStopped
  update (Reload n) = do
    delay 500
    pure $ viewPoll n

viewInit :: View Polling ()
viewInit = do
  row id $ do
    button (Reload 1) btn "Start Polling"

viewStopped :: View Polling ()
viewStopped = do
  row id $ do
    button (Reload 1) btn "Restart Polling"

viewPaused :: Int -> View Polling ()
viewPaused n = do
  col (gap 10) $ do
    row id $ do
      button (Reload n) btn "Resume"
    viewStatus n

viewPoll :: Int -> View Polling ()
viewPoll n = do
  -- reload every 200ms + round trip delay
  col (onLoad (Reload (n + 1)) 200 . gap 10) $ do
    row (gap 5) $ do
      button (Pause n) btn "Pause"
      button Stop btn "Stop"
    viewStatus n

viewStatus :: Int -> View Polling ()
viewStatus n = do
  el id $ do
    text "Polling... "
    text $ pack $ show n

-----------------------------------------------------------
-- Lazy Loading Expensive Data
-----------------------------------------------------------

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
  el (onLoad Details 100 . bg GrayLight . textAlign AlignCenter) $ do
    text "..."

viewTaskDetails :: Task -> View LazyData ()
viewTaskDetails task = do
  row (color Success) $ do
    text task.details

-- Fake Tasks Effect ----------------------------------------

type TaskId = Text

data Task = Task
  { taskId :: TaskId
  , details :: Text
  }

pretendLoadTask :: (Debug :> es, GenRandom :> es) => TaskId -> Eff es Task
pretendLoadTask taskId = do
  -- pretend it takes a little time to load
  randomDelay <- genRandom (100, 1000)
  delay randomDelay

  pure $ Task taskId $ "Details for " <> taskId

pretendTasks :: [TaskId]
pretendTasks = fmap (pack . show @Int) [1 .. 100]

fwrap :: Mod c
fwrap =
  addClass $
    cls "wrap"
      & prop @Text "flex-wrap" "wrap"
