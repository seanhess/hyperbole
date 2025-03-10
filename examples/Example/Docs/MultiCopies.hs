{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Docs.MultiCopies where

import Data.Text (Text)
import Web.Hyperbole

page :: Eff es (Page '[Message])
page = do
  pure $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

data Message = Message1 | Message2
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Generic, ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  row (gap 10) $ do
    button (Louder m) (border 1 . pad 5) "Louder"
    el (pad 5) $ text m
