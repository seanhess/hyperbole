{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Docs.BasicPage where

import Data.Text (Text)
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)

page :: Page es '[]
page = do
  pure $ do
    el "Hello World"

messageView :: Text -> View context ()
messageView msg =
  el $ text msg

helloWorld :: View context ()
helloWorld =
  el "Hello World"

page' :: Page es '[]
page' = do
  pure $ do
    messageView "Hello World"
