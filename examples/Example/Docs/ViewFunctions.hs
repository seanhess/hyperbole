{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.ViewFunctions where

import Data.Text (Text)
import Web.Hyperbole

page :: Eff es (Page '[Message])
page = do
  pure $ do
    hyper Message $ messageView "Hello"

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage t) =
    pure $ messageView t

messageView :: Text -> View Message ()
messageView m = do
  header m
  messageButton "Salutations!"
  messageButton "Good Morning!"
  messageButton "Goodbye"

messageButton :: Text -> View Message ()
messageButton msg = do
  button (SetMessage msg) (border 1) (text $ "Say " <> msg)

header :: Text -> View context ()
header txt = do
  el bold (text txt)
