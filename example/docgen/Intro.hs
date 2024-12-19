module Intro where

import Data.Text (Text)
import Web.Hyperbole


main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage)


messagePage :: Page es '[]
messagePage = do
  pure $ do
    el bold "Hello World"


messageView :: Text -> View c ()
messageView m = do
  el_ "Message:"
  el_ (text m)
