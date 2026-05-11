{-# LANGUAGE TemplateHaskell #-}

module App.Page.Test where

import App.Docs
import App.Route
import Data.Aeson (FromJSON (..), Value (..))
import Data.String.Conversions
import Data.Text (Text)
import Example.View.Layout
import Text.Read (readMaybe)
import Web.Atomic.CSS
import Web.Hyperbole hiding (Number)
import Web.Hyperbole.HyperView.Event

source :: ModuleSource
source = $(moduleSource)

page :: (Hyperbole :> es) => Page es '[FunkyEncodings, NumInput, StrInput]
page = do
  pure $ layout (Test TestMain) $ do
    section' "Encodings" $ do
      example source $ do
        hyper FunkyEncodings viewFunkyEncodings

      example source $ do
        hyper NumInput (viewNumInput Nothing)

      example source $ do
        hyper StrInput (viewStrInput "")

-- Encoding Tests

data FunkyEncodings = FunkyEncodings
  deriving (Generic, ViewId)

data Options
  = Something
  | Multi Int
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

instance HyperView FunkyEncodings es where
  data Action FunkyEncodings
    = Funky Int Text Text
    | Opts Options
    deriving (Generic, ViewAction, Show)

  update a =
    pure $ do
      viewFunkyEncodings
      el $ text $ cs $ show a

viewFunkyEncodings :: View FunkyEncodings ()
viewFunkyEncodings = do
  -- dropdown (\s -> Funky 10 s "hello big_world !") "" ~ border 1 $ do
  --   option "" "Select"
  --   option "   " "Spaces"
  --   option "Big Bear" "Big Bear"
  --   option "Nice_Option" "Nice_Option"

  -- dropdown (Funky 14 "first") "" ~ border 1 $ do
  --   option "" "Last Position"
  --   option "   " "Spaces"
  --   option "Big Bear" "Big Bear"

  dropdown Opts Something ~ border 1 $ do
    option Something "Something"
    option (Multi 1) "One"
    option (Multi 2) "Two"
    option (Multi 3) "Three"

  search (\s -> Funky 18 s "last one") 250 ~ border 1 . pad 5

newtype UserNum = UserNum Int
  deriving (Generic)
  deriving newtype (ToJSON)

-- WARNING: this is a bit misleading
-- we could use a custom encoding instead
--
-- Something "this is a test"
-- Something |>this is a test<|
-- Something |>Venus<|
-- Something |>Multi 33<|
instance FromJSON UserNum where
  parseJSON (Number n) = pure $ UserNum $ round n
  parseJSON (String t) =
    case readMaybe (cs t) of
      Nothing -> fail $ "not an integer: " <> cs t
      Just n -> pure $ UserNum n
  parseJSON _other = fail "OTHER"

---------------------------------------------------

data NumInput = NumInput
  deriving (Generic, ViewId)

instance HyperView NumInput es where
  data Action NumInput
    = GoNum Int Text UserNum
    deriving (Generic, ViewAction)

  update (GoNum _ _ (UserNum mn)) = do
    pure $ viewNumInput $ Just mn

viewNumInput :: Maybe Int -> View NumInput ()
viewNumInput mn = do
  row ~ gap 10 $ do
    tag "input" @ onChange (GoNum 55 "hello world") . placeholder "Enter an integer" ~ border 1 . pad 10 $ none
    case mn of
      Nothing -> "_"
      Just n -> el $ text $ cs $ show n

data StrInput = StrInput
  deriving (Generic, ViewId)

instance HyperView StrInput es where
  data Action StrInput
    = GoInput Text
    deriving (Generic, ViewAction)

  update (GoInput t) = do
    pure $ viewStrInput t

viewStrInput :: Text -> View StrInput ()
viewStrInput inp = do
  row ~ gap 10 $ do
    tag "input" @ onChange GoInput . placeholder "Enter some text" ~ border 1 . pad 10 $ none
    el $ text inp
