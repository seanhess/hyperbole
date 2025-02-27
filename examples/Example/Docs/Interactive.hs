module Example.Docs.Interactive where

import Data.Text (Text)
import Web.Hyperbole

page :: Eff es (Page '[Message])
page = do
  pure $ do
    col (pad 10 . gap 10) $ do
      el (bold . fontSize 24) "Unchanging Header"
      hyper Message $ messageView "Hello World"

messageView :: Text -> View Message ()
messageView msg = do
  el bold $ text msg
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)

  update (SetMessage msg) =
    pure $ messageView msg
