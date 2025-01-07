{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Docs.BasicPage where

import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage)

messagePage :: Eff es (Page '[])
messagePage = do
  pure $ do
    col (pad 10) $ do
      el bold "Hello World"
