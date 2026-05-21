{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Concurrency.Overlap where

import App.Docs
import Data.Text (Text, pack)
import Effectful
import Example.Effects.Debug
import Example.Style.Cyber (btn)
import Example.View.Loader as Loader
import Web.Atomic.CSS
import Web.Hyperbole

-- Concurrency = Drop  ---------------------------

data OverlapDrop = OverlapDrop
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView OverlapDrop es where
  data Action OverlapDrop
    = GetTimeDrop
    deriving (Generic, ViewAction)

  -- this is the default, not necessary to specify
  -- type Concurrency OverlapDrop = Drop

  update GetTimeDrop = do
    t <- getTimeSlowly
    pure $ viewTimeDrop (Just t)

viewTimeDrop :: Maybe UTCTime -> View OverlapDrop ()
viewTimeDrop = viewTime GetTimeDrop "Drop"

-- Concurrency = Replace  --------------------------

data OverlapReplace = OverlapReplace
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView OverlapReplace es where
  data Action OverlapReplace
    = GetTimeReplace
    deriving (Generic, ViewAction)

  type Concurrency OverlapReplace = Replace

  update GetTimeReplace = do
    t <- getTimeSlowly
    pure $ viewTimeReplace (Just t)

viewTimeReplace :: Maybe UTCTime -> View OverlapReplace ()
viewTimeReplace = viewTime GetTimeReplace "Replace"

-- Utilities -----------------------------------------------

getTimeSlowly :: (Debug :> es) => Eff es UTCTime
getTimeSlowly = do
  delay 2000
  systemTime

viewTime :: (ViewAction (Action id)) => Action id -> Text -> Maybe UTCTime -> View id ()
viewTime runTime loadLbl mtime = do
  row ~ gap 10 $ do
    button runTime ~ btn $ text loadLbl
    Loader.loading
    case mtime of
      Nothing -> none
      Just t -> el ~ whenLoading (display None) $ text $ pack $ show t

source :: ModuleSource
source = $(moduleSource)
