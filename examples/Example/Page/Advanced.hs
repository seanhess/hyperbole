module Example.Page.Advanced where

import Data.Text (Text)
import Docs.Page
import Example.AppRoute as Route
import Example.Style.Cyber as Cyber (btn, font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Message, Controls]
page = do
  pure $ layout Route.Advanced $ do
    section' "Trigger" $ do
      el "Tell other HyperViews to run an action"
      example Advanced $ do
        hyper Message $ messageView "..."

      example Advanced $ do
        hyper Controls controlView

    section' "Target" $ do
      el "Alternatively, you can switch contexts to another view to embed its actions"

      example Advanced $ do
        hyper Controls targetView

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage msg) = do
    pure $ messageView msg

messageView :: Text -> View Message ()
messageView msg = do
  el ~ pad 10 . border 1 . Cyber.font $ do
    text msg

data Controls = Controls
  deriving (Generic, ViewId)

instance HyperView Controls es where
  type Require Controls = '[Message]

  data Action Controls = TriggerMessage
    deriving (Generic, ViewAction)

  update TriggerMessage = do
    trigger Message $ SetMessage "Triggered!"
    pure controlView

controlView :: View Controls ()
controlView = do
  button TriggerMessage ~ btn $ "Trigger Message"

targetView :: View Controls ()
targetView = do
  target Message () $ do
    button (SetMessage "Targeted!") ~ btn $ "Target SetMessage"
