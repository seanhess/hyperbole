{-# LANGUAGE TemplateHaskell #-}

module Example.Javascript where

import Data.Text (Text, pack)
import Docs.Examples
import Example.Interactivity.Events (box, viewBoxes')
import Web.Atomic.CSS
import Web.Hyperbole

data Boxes = Boxes
  deriving (Generic, ViewId)

instance HyperView Boxes es where
  data Action Boxes
    = Selected Int
    | Clear
    deriving (Generic, ViewAction)

  type Concurrency Boxes = Replace

  update (Selected n) = do
    pure $ viewBoxes (Just n)
  update Clear = do
    pure $ viewBoxes Nothing

viewBoxes :: Maybe Int -> View Boxes ()
viewBoxes mn = do
  viewBoxes' mn $ \n -> do
    el ~ box . cls "box" $ text $ pack $ show n

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = AlertMe
    deriving (Generic, ViewAction)

  update AlertMe = do
    pushEvent @Text "server-message" "hello"
    pure "Sent 'server-message' event"

source :: ModuleSource
source = $(moduleSource)
