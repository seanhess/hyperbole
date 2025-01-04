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
    el_ "This part will not be updated (after initial page load)"
    hyper Message $ messageView "Hello"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)

  update (SetMessage m) =
    pure $ messageView m

messageView' :: Text -> View Message ()
messageView' m = do
  header $ "Message: " <> m
  goodbyeButton

goodbyeButton :: View Message ()
goodbyeButton = do
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

header :: Text -> View c ()
header txt = do
  el bold (text txt)
