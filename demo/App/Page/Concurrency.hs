{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Concurrency where

import App.Docs
import App.Route qualified as Route
import Control.Monad (forM_)
import Effectful
import Example.Concurrency.LazyLoading as Lazy
import Example.Concurrency.Overlap as Overlap
import Example.Concurrency.Polling as Polling
import Example.Concurrency.Progress as Progress
import Example.Concurrency.Tasks
import Example.Effects.Debug
import Example.Push qualified as Push
import Example.Style.Cyber (font)
import Example.View.Layout (layoutSubnav)
import Example.View.Loader as Loader
import Web.Atomic.CSS
import Web.Hyperbole

data Section
  = Concurrency
  | OverlappingRequests
  | LazyLoading
  | Polling
  | PushUpdates
  deriving (Show, Eq, Enum, Bounded, PageAnchor)

page :: (Hyperbole :> es, Debug :> es) => Page es '[Poller, LazyData, Progress, Push.Tasks, OverlapDrop, OverlapReplace]
page = do
  pure $ layoutSubnav @Section Route.Concurrency $ do
    style Loader.css
    section Concurrency $ do
      markdocs "While individual `HyperView`s can only have one update in progress at a time, multiple `HyperView`s can overlap updates without issue"
      example Progress.source ~ font $ do
        hyper (Progress 1) $ viewProgressLoad 6
        hyper (Progress 2) $ viewProgressLoad 4
        hyper (Progress 3) $ viewProgressLoad 2
    -- hyper (Progress 4 200) viewProgressLoad
    -- hyper (Progress 5 250) viewProgressLoad

    section OverlappingRequests $ do
      markdocs $(embedFile "docs/concurrency-overlap.md")

      example $(moduleSourceNamed "Example.Concurrency.Overlap") $ do
        hyper OverlapDrop $ viewTimeDrop Nothing
        hyper OverlapReplace $ viewTimeReplace Nothing

    section LazyLoading $ do
      markdocs "Instead of preloading everything in our `Page`, a `HyperView` can load itself using `onLoad`"
      snippet $ raw $(embedTopLevel "Example.Concurrency.LazyLoading" "viewTaskLoad")
      example Lazy.source $ do
        row ~ flexWrap Wrap . font . gap 10 $ do
          forM_ pretendTasks $ \taskId -> do
            el ~ border 1 . width 120 . pad 5 $ do
              hyper (LazyData taskId) viewTaskLoad

    section Polling $ do
      markdocs "By including an `onLoad` in every view update, we can poll the server after a given delay"
      snippet $ raw $(embedTopLevel "Example.Concurrency.Polling" "viewPoll")
      example Polling.source $ do
        hyper Poller viewInit

    section PushUpdates $ do
      markdocs "Actions can call `pushUpdate` to send an intermediate update to the view. This can be simpler than polling. If another"
      snippet $ raw $(embedTopLevel "Example.Push" "update")
      example Push.source $ do
        hyper Push.Tasks $ Push.taskView 0
