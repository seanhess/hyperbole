{-# LANGUAGE TemplateHaskell #-}

module Example.Javascript where

import Data.Text (Text, pack)
import App.Docs
import Example.Interactivity.Events (box, boxes)
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[JBoxes, Message]
page = do
  pure $ do
    script "custom.js"
    hyper JBoxes $ viewJBoxes Nothing
    hyper Message viewMessage

data JBoxes = JBoxes
  deriving (Generic, ViewId)

instance HyperView JBoxes es where
  data Action JBoxes
    = Selected Int
    | Clear
    deriving (Generic, ViewAction)

  type Concurrency JBoxes = Replace

  update (Selected n) = do
    pure $ viewJBoxes (Just n)
  update Clear = do
    pure $ viewJBoxes Nothing

viewJBoxes :: Maybe Int -> View JBoxes ()
viewJBoxes mn = do
  boxes mn $ \n -> do
    el ~ box . cls "box" $ text $ pack $ show n

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = AlertMe
    deriving (Generic, ViewAction)

  update AlertMe = do
    pushEvent "server-message" ("hello" :: Text)
    pure "Sent 'server-message' event"

viewMessage :: View Message ()
viewMessage = do
  button AlertMe ~ btn $ "Alert Me"

source :: ModuleSource
source = $(moduleSource)
