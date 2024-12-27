{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Intro.BasicPage where

import Web.Hyperbole


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage)


messagePage :: Eff es (Page '[])
messagePage = do
  pure $ do
    el bold "Hello World"
