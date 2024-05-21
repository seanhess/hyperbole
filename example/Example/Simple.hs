{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Simple where

import Data.Text (Text)
import Web.Hyperbole


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (page simplePage)


simplePage :: (Hyperbole :> es) => Page es Response
simplePage = do
  handle message
  load $ do
    pure $ col (pad 20) $ do
      el bold "My Page"
      hyper (Message 1) $ messageView "Hello"
      hyper (Message 2) $ messageView "World!"


data Message = Message Int
  deriving (Generic, Param)


data MessageAction = Louder Text
  deriving (Generic, Param)


instance HyperView Message where
  type Action Message = MessageAction


message :: Message -> MessageAction -> Eff es (View Message ())
message _ (Louder m) = do
  let new = m <> "!"
  pure $ messageView new


messageView m = do
  el_ $ text m
  button (Louder m) (border 1) "Louder"
