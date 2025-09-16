{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Test where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Example.AppRoute
import Example.Effects.Debug
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.Param

-- TEST: add a test for Page+trigger
page :: (Hyperbole :> es, Debug :> es) => Page es '[Chooser]
page = do
  -- trigger Fake Noop
  pure $ exampleLayout Test $ do
    -- script "test.js"
    example Test $ do
      col ~ embed $ do
        hyper Chooser chooserView

data Woot = Woot | Boot
  deriving (Eq, Generic, Show, ToParam, FromParam)

data Tag = A | Tag Text
  deriving (Eq, Generic, Show, ToParam, FromParam)

data Custom = C1 | C2 Text
  deriving (Eq, Show)

instance ToParam Custom where
  toParam C1 = "c1"
  toParam (C2 t) =
    let out = "c2|" <> t
     in ParamValue out

instance FromParam Custom where
  parseParam (ParamValue t)
    | "c1" <- t = pure C1
    | T.isPrefixOf "c2" t = pure $ C2 $ T.drop 3 t
    | otherwise = Left $ "Could not parse" <> cs t

data Chooser = Chooser
  deriving (Generic, ViewId)

instance HyperView Chooser es where
  data Action Chooser
    = Choose (Maybe Woot)
    | Choose2 Tag
    | ChooseCustom Custom
    | SetMessage Text
    deriving (Generic, ViewAction)

  update (Choose mw) = do
    pure $ do
      el $ text $ cs $ show mw
  update (Choose2 t) = do
    pure $ do
      el $ text $ cs $ show t
  update (ChooseCustom t) = do
    pure $ do
      el $ text $ cs $ show t
  update (SetMessage t) = do
    pure $ messageView t

chooserView :: View Chooser ()
chooserView = do
  col ~ gap 10 $ do
    el "Chooser"
    button (SetMessage "woo") ~ border 1 $ "woo"
    button (SetMessage "") ~ border 1 $ "clear"
    dropdown Choose (Just Woot) $ do
      option Nothing "Choose One"
      option (Just Woot) "Woot"
      option (Just Boot) "Boot"
    dropdown Choose2 (Tag "hello") $ do
      option A "A"
      option (Tag "hello") "hello"
      option (Tag "world") "world"
      option (Tag "hello world") "hello world"
    dropdown ChooseCustom (C2 "hello") $ do
      option C1 "C1"
      option (C2 "hello") "hello"
      option (C2 "world") "world"
      option (C2 "hello world") "hello world"

messageView :: Text -> View Chooser ()
messageView t = do
  el $ text $ "(" <> t <> ")"

-- data Message = Message1 | Message2
--   deriving (Generic, ViewId, Show)
--
-- instance (Debug :> es) => HyperView Message es where
--   data Action Message
--     = Louder Text
--     deriving (Generic, ViewAction)
--
--   update (Louder m) = do
--     v :: Message <- viewId
--     traceM $ "Updating " <> show v
--     let new = m <> "!"
--     delay 1000
--     traceM $ " - done " <> show v
--     pure $ messageView new
--
-- messageView :: Text -> View Message ()
-- messageView m = do
--   row ~ gap 10 $ do
--     button (Louder m) ~ border 1 . pad 5 . whenLoading (borderColor Danger . color Danger) $ "Louder"
--     el ~ pad 5 $ text m
--
-- data Other = Other
--   deriving (Generic, ViewId)
--
-- instance (Debug :> es) => HyperView Other es where
--   type Require Other = '[Message]
--
--   data Action Other
--     = GoTrigger
--     | Sneaky
--     deriving (Generic, ViewAction)
--
--   update GoTrigger = do
--     -- trigger Fake Noop
--     trigger Message2 (Louder "remote")
--     pushEvent "hello" (String "woot")
--     pure "OK"
--   update Sneaky = do
--     pure "Sneaky"
--
-- ---- NOT ON PAGE ---------------------------
--
-- data Fake = Fake
--   deriving (Generic, ViewId)
--
-- instance HyperView Fake es where
--   data Action Fake
--     = Noop
--     deriving (Generic, ViewAction)
--
--   update _ = do
--     pure "OK"
