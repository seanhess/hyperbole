{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Simple where

import Data.Text (Text)
import Web.Hyperbole


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage simplePage)


simplePage :: (Hyperbole :> es) => Page es '[Message]
simplePage = do
  pure $ col (pad 20) $ do
    el bold "My Page"
    hyper (Message 1) $ messageView "Hello"
    hyper (Message 2) $ messageView "World!"


data Message = Message Int
  deriving (Show, Read, ViewId)


instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)
  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new


messageView m = do
  el_ $ text m
  button (Louder m) (border 1) "Louder"
