{-# LANGUAGE TemplateHaskell #-}

module App.Page.Test where

import App.Docs
import App.Route
import Control.Monad (forM_)
import Data.String.Conversions
import Data.Text (Text)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole hiding (Number)
import Web.Hyperbole.HyperView.Event (onChange)

source :: ModuleSource
source = $(moduleSource)

page :: (Hyperbole :> es) => Page es '[Changes]
page = do
  pure $ layout (Test TestMain) $ do
    section' "Encodings" $ do
      -- example source $ do
      --   hyper FunkyEncodings viewFunkyEncodings

      example source $ do
        hyper Changes (viewChanges "")

-- Encoding Tests

-- data FunkyEncodings = FunkyEncodings
--   deriving (Generic, ViewId)

data Options
  = Something
  | Multi Text Int
  deriving (Generic, ToJSON, FromJSON, Show, Eq, InputValue)

-- instance HyperView FunkyEncodings es where
--   data Action FunkyEncodings
--     = Funky Int Text
--     | Opts Options
--     deriving (Generic, ViewAction, Show)
--
--   update a =
--     pure $ do
--       viewFunkyEncodings
--       el $ text $ cs $ show a
--
-- viewFunkyEncodings :: View FunkyEncodings ()
-- viewFunkyEncodings = do
--   -- dropdown (\s -> Funky 10 s "hello big_world !") "" ~ border 1 $ do
--   --   option "" "Select"
--   --   option "   " "Spaces"
--   --   option "Big Bear" "Big Bear"
--   --   option "Nice_Option" "Nice_Option"
--
--   -- dropdown (Funky 14 "first") "" ~ border 1 $ do
--   --   option "" "Last Position"
--   --   option "   " "Spaces"
--   --   option "Big Bear" "Big Bear"
--
--   dropdown Opts Something ~ border 1 $ do
--     option Something "Something"
--     option (Multi "" 1) "One"
--     option (Multi "" 2) "Two"
--     option (Multi "" 3) "Three"
--
--   search (\s -> Funky 18 s "last one") 250 ~ border 1 . pad 5

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
-- instance FromJSON UserNum where
--   parseJSON (Number n) = pure $ UserNum $ round n
--   parseJSON (String t) =
--     case readMaybe (cs t) of
--       Nothing -> fail $ "not an integer: " <> cs t
--       Just n -> pure $ UserNum n
--   parseJSON _other = fail "OTHER"

---------------------------------------------------

data Changes = Changes
  deriving (Generic, ViewId)

instance HyperView Changes es where
  data Action Changes
    = GoNum Int Text
    | GoInput
    | GoOption
    | GoNumOption
    deriving (Generic, ViewAction)

  update (GoNum _ _) = do
    n <- inputValue @Int
    pure $ viewChanges (cs $ show n)
  update GoInput = do
    t <- inputValue
    pure $ viewChanges t
  update GoOption = do
    o <- inputValue @Options
    pure $ do
      viewChanges (cs $ show o)
  update GoNumOption = do
    n <- inputValue @Int
    pure $ do
      viewChanges (cs $ show n)

viewChanges :: Text -> View Changes ()
viewChanges inp = do
  col ~ gap 10 $ do
    el ~ border 1 . pad 20 . bold $ text inp

    row ~ gap 10 $ do
      tag "input" @ onChange (GoNum 55 "hello world") . placeholder "Enter an integer" ~ border 1 . pad 10 $ none

    row ~ gap 10 $ do
      tag "input" @ onChange GoInput . placeholder "Enter some text" ~ border 1 . pad 10 $ none

    dropdown GoOption Something ~ border 1 $ do
      option Something "Something"
      option (Multi "one" 1) "One"
      option (Multi "two!" 2) "Two"
      option (Multi "hello world" 3) "Three"

    dropdown GoNumOption (0 :: Int) ~ border 1 $ do
      forM_ [0 .. 10] $ \n -> do
        option n (cs $ show n)
