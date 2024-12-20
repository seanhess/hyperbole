{-# LANGUAGE UndecidableInstances #-}

module Intro where

import Data.Text (Text)
import Web.Hyperbole


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage)


messagePage :: Eff es (Page '[])
messagePage = do
  pure $ do
    el bold "Hello World"


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


  update (SetMessage t) =
    pure $ el_ (text t)


messagePage' :: Eff es (Page '[Message])
messagePage' = do
  pure $ do
    hyper Message $ do
      el bold "Hello World"
      button (SetMessage "Goodbye") (border 1) "Say Goodbye"
