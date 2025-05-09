{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Docs.BasicPage where

import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: Eff es (Page '[])
page = do
  pure $ do
    col ~ pad 10 $ do
      el ~ bold $ "Hello World"

messageView :: Text -> View context ()
messageView msg =
  el ~ bold $ (text msg)

helloWorld :: View context ()
helloWorld =
  el ~ bold $ "Hello World"

page' :: Eff es (Page '[])
page' = do
  pure $ do
    col ~ pad 10 $ do
      messageView "Hello World"
