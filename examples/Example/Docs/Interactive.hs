module Example.Docs.Interactive where

import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

page :: Page es '[Message]
page = do
  pure $ do
    el "Unchanging Header"
    hyper Message $ messageView "Hello World"

messageView :: Text -> View Message ()
messageView msg = do
  el ~ bold $ text msg
  button (SetMessage "Goodbye") "Say Goodbye"

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage msg) =
    pure $ messageView msg
