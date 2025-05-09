{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.Simple where

import Data.Text (Text)
import Example.AppRoute qualified as Route
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  pure $ exampleLayout Route.Simple $ col ~ pad 20 . gap 10 $ do
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
  row ~ gap 10 $ do
    button (Louder m) ~ border 1 . pad 5 $ "Louder"
    el ~ pad 5 $ text m
