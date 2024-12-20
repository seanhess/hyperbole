{-# LANGUAGE UndecidableInstances #-}

module IntroMulti where

import Data.Text (Text)
import Web.Hyperbole


messageView :: Text -> View Message ()
messageView m = do
  el bold $ text $ "Message: " <> m
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"


messagePage :: Eff es (Page '[Message])
messagePage = do
  pure $ do
    hyper (Message 1) $ messageView "Hello"
    hyper (Message 2) $ messageView "World"


data Message = Message Int
  deriving (Show, Read, ViewId)


instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)


  update (SetMessage t) =
    pure $ messageView t
