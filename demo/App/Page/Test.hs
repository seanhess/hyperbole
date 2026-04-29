{-# LANGUAGE TemplateHaskell #-}

module App.Page.Test where

import App.Docs
import App.Route
import Data.String.Conversions
import Data.Text (Text)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

source :: ModuleSource
source = $(moduleSource)

page :: (Hyperbole :> es) => Page es '[FunkyEncodings]
page = do
  pure $ layout (Test TestMain) $ do
    section' "Encodings" $ do
      example source $ do
        hyper FunkyEncodings viewFunkyEncodings

-- Encoding Tests

data FunkyEncodings = FunkyEncodings
  deriving (Generic, ViewId)

instance HyperView FunkyEncodings es where
  data Action FunkyEncodings
    = Funky Int Text Text
    deriving (Generic, ViewAction, Show)

  update a =
    pure $ do
      viewFunkyEncodings
      el $ text $ cs $ show a

viewFunkyEncodings :: View FunkyEncodings ()
viewFunkyEncodings = do
  dropdown (\s -> Funky 10 s "hello big_world !") "" ~ border 1 $ do
    option "" "Select"
    option "   " "Spaces"
    option "Big Bear" "Big Bear"
    option "Nice_Option" "Nice_Option"

  dropdown (Funky 14 "first") "" ~ border 1 $ do
    option "" "Last Position"
    option "   " "Spaces"
    option "Big Bear" "Big Bear"

  search (\s -> Funky 18 s "last one") 250 ~ border 1 . pad 5
