{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Simple where

import Data.Text (pack)
import Effectful
import Web.Hyperbole


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (page simplePage)


simplePage :: (Hyperbole :> es, IOE :> es) => Page es (MainView, Status)
simplePage = do
  handle (main', status) $ do
    liftIO $ putStrLn "MAIN LOAD"
    pure $ col (pad 20) $ do
      el bold "My Page"
      hyper MainView $ do
        row (gap 10) $ do
          button GoBegin (border 1) "Start"


simplePage1 :: (Hyperbole :> es, IOE :> es) => Page es Floop
simplePage1 = do
  handle floop $ do
    liftIO $ putStrLn "MAIN LOAD"
    pure $ col (pad 20) $ do
      el bold "My Page"
      hyper Floop $ do
        row (gap 10) $ do
          button FloopA (border 1) "Start"


simplePage0 :: (Hyperbole :> es, IOE :> es) => Page es ()
simplePage0 = do
  handle () $ do
    liftIO $ putStrLn "MAIN LOAD"
    pure $ col (pad 20) $ do
      el bold "My Page"


data Floop = Floop
  deriving (Show, Read, ViewId)


data FloopA = FloopA
  deriving (Show, Read, ViewAction)


instance HyperView Floop where
  type Action Floop = FloopA


floop :: Floop -> FloopA -> Eff es (View Floop ())
floop _ _ = pure none


-- MAIN ----------------------------------------

data MainView = MainView
  deriving (Show, Read, ViewId)


data MainAction
  = GoBegin
  | GoMid
  | GoEnd
  deriving (Show, Read, ViewAction)


instance HyperView MainView where
  type Action MainView = MainAction
  type Require MainView = '[Status]


main' :: MainView -> MainAction -> Eff es (View MainView ())
main' _ = \case
  GoBegin -> pure beginStep
  GoMid -> pure middleStep
  GoEnd -> pure endStep


beginStep :: View MainView ()
beginStep = do
  el_ "BEGIN"
  button GoMid (border 1) " Mid"


middleStep :: View MainView ()
middleStep = do
  el_ "MIDDLE: running"
  button GoBegin (border 1) "Back"
  hyper Status $ statusView 0


endStep :: View MainView ()
endStep = do
  el_ "END"
  button GoMid (border 1) "Back"


-- Status ---------------------------------------

data Status = Status
  deriving (Show, Read, ViewId)


data CheckStatus
  = CheckStatus Int
  deriving (Show, Read, ViewAction)


instance HyperView Status where
  type Action Status = CheckStatus
  type Require Status = '[MainView]


status :: Status -> CheckStatus -> Eff es (View Status ())
status _ = \case
  CheckStatus n ->
    if n >= 5
      then pure lazyEnd
      else pure $ statusView (n + 1)


statusView :: Int -> View Status ()
statusView n = do
  onLoad (CheckStatus n) 1000 $ do
    el_ $ text $ "Checking Status" <> pack (show n)


lazyEnd :: View Status ()
lazyEnd = do
  el_ "Lazy End"
  hyper MainView $ do
    button GoEnd (border 1) "Go End"
