module Example.Intro.Interactive where

import Data.Text (Text)
import Web.Hyperbole

page :: Eff es (Page '[Message])
page = messagePage

messagePage :: Eff es (Page '[Message])
messagePage =
  pure $
    col (pad 10 . gap 10) $
      hyper Message $
        messageView "Hello World"

messageView :: Text -> View Message ()
messageView m = do
  el bold $ text $ "Message: " <> m
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)

  update (SetMessage m) = pure $ messageView m
