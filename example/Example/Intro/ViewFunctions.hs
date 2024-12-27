{-# LANGUAGE UndecidableInstances #-}

module Example.Intro.ViewFunctions where

import Data.Text (Text)
import Web.Hyperbole

messageView :: Text -> View Message ()
messageView m = do
  el bold $ text $ "Message: " <> m
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

messagePage :: Eff es (Page '[Message])
messagePage = do
  pure $ do
    hyper Message $ messageView "Hello"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)

  update (SetMessage t) =
    pure $ messageView t
