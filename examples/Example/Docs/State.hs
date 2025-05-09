{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.State where

import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) ~ border 1 $ "Louder"
  el ~ bold $ text m

page :: Eff es (Page '[Message])
page = do
  pure $ do
    hyper Message $ messageView "Hello"

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new
