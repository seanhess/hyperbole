module Example.Triggers where

import Data.Text (Text)
import Effectful
import Effectful.Concurrent.STM
import Example.Style as Style
import Web.Hyperbole
import Web.Hyperbole.Effect.Hyperbole


page :: (Hyperbole :> es, Concurrent :> es) => Page es '[Controls, Message]
page = do
  pure $ col (pad 20 . gap 10) $ do
    el bold "Triggers"

    row (gap 10) $ do
      hyper Controls viewControls
      hyper (Message 0) $ viewMessage "ready"
      hyper (Message 1) $ viewMessage "ready"
      hyper (Message 2) $ viewMessage "ready"


data Controls = Controls
  deriving (Show, Read, ViewId)


data ControlsAction
  = Broadcast Text
  | Clear
  deriving (Show, Read, ViewAction)


instance HyperView Controls where
  type Action Controls = ControlsAction
  type Require Controls = '[Message]
instance Handle Controls es where
  handle (Broadcast t) = do
    -- `trigger` does not guarantee that the view exists on the page
    trigger (Message 0) (Say t)
    trigger (Message 1) (Say t)
    trigger (Message 2) (Say t)

    -- Error: HyperView NotHandledView not found in (Require Controls)
    -- trigger NotHandledView OhNo

    pure viewControls
  handle Clear = do
    trigger (Message 0) (Say "")
    trigger (Message 1) (Say "")
    trigger (Message 2) (Say "")
    pure viewControls


viewControls :: View Controls ()
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


-- If you try to trigger an action for a view that isn't handled in your page you will get an error

data NotHandledView = NotHandledView
  deriving (Show, Read, ViewId)


data OhNo
  = OhNo
  deriving (Show, Read, ViewAction)


instance HyperView NotHandledView where
  type Action NotHandledView = OhNo
