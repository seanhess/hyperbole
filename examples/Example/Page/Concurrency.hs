{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Concurrency where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Effectful
import Example.AppRoute
import Example.Colors
import Example.Effects.Debug
import Example.Style.Cyber (btn, font)
import Example.View.Inputs (progressBar)
import Example.View.Layout (embed, example', exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.Response (pushUpdateTo)

page :: (Hyperbole :> es, Debug :> es) => Page es '[Polling, LazyData, Progress, Tasks]
page = do
  pure $ exampleLayout Concurrency $ do
    example' "Concurrency" source $ do
      el "While individual HyperViews can only have one update in progress at a time, multiple HyperViews can overlap updates without issue"
      el ~ embed . font $ do
        hyper (Progress 1 100) $ viewProgress 0
        hyper (Progress 2 200) $ viewProgress 0
        hyper (Progress 3 300) $ viewProgress 0
        hyper (Progress 4 400) $ viewProgress 0
        hyper (Progress 5 500) $ viewProgress 0

    example' "Lazy Loading" source $ do
      el $ do
        text "Instead of preloading everything in our Page, a HyperView can load itself using "
        code "onLoad"
      el ~ flexRow . embed . flexWrap Wrap . font $ do
        forM_ pretendTasks $ \taskId -> do
          el ~ border 1 . width 120 . pad 5 $ do
            hyper (LazyData taskId) viewTaskLoad

    example' "Polling" source $ do
      el $ do
        text "By including an "
        code "onLoad"
        text "in every view update, we can poll the server after a given delay"
      col ~ embed $ hyper Polling viewInit

    example' "Push Updates" source $ do
      el $ do
        text "Actions can call "
        code "pushUpdate"
        text " to send an intermediate update to the view. This is simpler than polling"
      col ~ embed . font $ do
        hyper Tasks $ taskView 0
 where
  source = routeSource Concurrency

-----------------------------------------------------------
-- Simple Polling
-----------------------------------------------------------

data Polling = Polling
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView Polling es where
  data Action Polling
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

viewInit :: View Polling ()
viewInit = do
  row $ do
    button (Reload 1) "Start Polling" ~ btn

viewStopped :: View Polling ()
viewStopped = do
  row $ do
    button (Reload 1) "Restart Polling" ~ btn

viewPaused :: Int -> View Polling ()
viewPaused n = do
  col ~ gap 10 $ do
    row $ do
      button (Reload n) "Resume" ~ btn
    viewStatus n

viewPoll :: Int -> View Polling ()
viewPoll n = do
  -- reload every 200ms + round trip delay
  col @ onLoad (Reload (n + 1)) 250 ~ gap 10 . loading $ do
    row ~ gap 5 $ do
      button (Pause n) "Pause" ~ btn
      button Stop "Stop" ~ btn
    viewStatus n

viewStatus :: Int -> View Polling ()
viewStatus n = do
  el $ do
    text "Polling... "
    text $ pack $ show n

-----------------------------------------------------------
-- Lazy Loading Expensive Data
-----------------------------------------------------------

data LazyData = LazyData TaskId
  deriving (Generic, ViewId)

instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
  data Action LazyData
    = Details
    deriving (Generic, ViewAction)

  update Details = do
    LazyData taskId <- viewId
    task <- pretendLoadTask taskId
    pure $ viewTaskDetails task

viewTaskLoad :: View LazyData ()
viewTaskLoad = do
  -- 100ms after rendering, get the details
  el @ onLoad Details 100 ~ bg GrayLight . textAlign AlignCenter $ do
    text "..."

viewTaskDetails :: Task -> View LazyData ()
viewTaskDetails task = do
  el ~ color Success . textAlign AlignCenter $ do
    text task.details

-- Fake Tasks Effect ----------------------------------------

type TaskId = Int

data Task = Task
  { taskId :: TaskId
  , details :: Text
  }

pretendLoadTask :: (Debug :> es, GenRandom :> es) => TaskId -> Eff es Task
pretendLoadTask taskId = do
  randomDelay <- genRandom (100, 1000)
  delay randomDelay

  pure $ Task taskId $ pack (show taskId)

pretendTasks :: [TaskId]
pretendTasks = [1 .. 30]

-----------------------------------------------------------

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

---------------------------------------------------------------

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
