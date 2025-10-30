{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Docs.BasicPage where

import Data.Text (Text)
import Web.Hyperbole

main :: IO ()
main = do
  app <- liveApp quickStartDocument (runPage page)
  run 3000 app

page :: Page es '[]
page = do
  pure $ el "Hello World"

messageView :: Text -> View context ()
messageView msg =
  el $ text msg

helloWorld :: View context ()
helloWorld =
  el "Hello World"

page' :: Page es '[]
page' = do
  pure $ messageView "Hello World"
