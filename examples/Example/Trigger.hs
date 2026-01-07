{-# LANGUAGE TemplateHaskell #-}

module Example.Trigger where

import Data.Text (Text)
import Docs
import Example.Style.Cyber as Cyber (btn, font)
import Web.Atomic.CSS
import Web.Hyperbole

data Targeted = Targeted
  deriving (Generic, ViewId)

instance HyperView Targeted es where
  data Action Targeted = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage msg) = do
    pure $ targetedView msg

targetedView :: Text -> View Targeted ()
targetedView msg = do
  el ~ pad 10 . border 1 . Cyber.font $ do
    text msg

data Controls = Controls
  deriving (Generic, ViewId)

instance HyperView Controls es where
  type Require Controls = '[Targeted]

  data Action Controls = TriggerMessage
    deriving (Generic, ViewAction)

  update TriggerMessage = do
    trigger Targeted $ SetMessage "Triggered!"
    pure controlView

controlView :: View Controls ()
controlView = do
  button TriggerMessage ~ btn $ "Trigger Message"

targetView :: View Controls ()
targetView = do
  target Targeted () $ do
    button (SetMessage "Targeted!") ~ btn $ "Target SetMessage"

source :: ModuleSource
source = $(moduleSource)
