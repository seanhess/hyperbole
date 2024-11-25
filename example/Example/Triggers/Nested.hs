module Example.Triggers.Nested where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent.STM
import Example.Style as Style
import Web.Hyperbole


page :: (Hyperbole :> es, Concurrent :> es) => Page es '[Content, Message]
page = do
  pure $ col (pad 20 . gap 10) $ do
    el bold "Triggers Nested"
    hyper Content $ viewContent Nothing


data Content = Content
  deriving (Show, Read, ViewId)


data ContentAction
  = Broadcast Text
  | Clear
  deriving (Show, Read, ViewAction)


instance HyperView Content where
  type Action Content = ContentAction
  type Require Content = '[Message]
instance Handle Content es where
  handle (Broadcast t) = do
    pure $ viewContent (Just t)
  handle Clear = do
    pure $ viewContent (Just "")


viewContent :: Maybe Text -> View Content ()
viewContent broadcast = do
  let msg = fromMaybe "ready" broadcast
  row (gap 10) $ do
    viewControls
    hyper (Message 0) $ viewMessage msg
    hyper (Message 1) $ viewMessage msg
    hyper (Message 2) $ viewMessage msg


viewControls :: View Content ()
viewControls = col (gap 5) $ do
  button Clear Style.btn "Clear"
  button (Broadcast "hello") Style.btn "Broadcast: hello"
  button (Broadcast "goodbye") Style.btn "Broadcast: goodbye"


data Message = Message Int
  deriving (Show, Read, ViewId)


data MessageAction
  = Say Text
  deriving (Show, Read, ViewAction)


instance HyperView Message where
  type Action Message = MessageAction
instance Handle Message es where
  handle (Say t) = do
    pure $ viewMessage t


viewMessage :: Text -> View Message ()
viewMessage msg = col (gap 10) $ do
  el (border 1 . textAlign Center) (text msg)
  row (gap 10) $ do
    button (Say "Hi") Style.btnLight "Say Hi"
    button (Say "Bye") Style.btnLight "Say Bye"
