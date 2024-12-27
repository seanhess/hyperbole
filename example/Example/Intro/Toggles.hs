module Example.Intro.Toggles where

import Data.Text (Text)
import Web.Hyperbole

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) (border 1) "Louder"
  el bold $ text $ "Message: " <> m

messagePage :: Eff es (Page '[Message])
messagePage = do
  pure $ do
    hyper Message $ messageView "Hello"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new
