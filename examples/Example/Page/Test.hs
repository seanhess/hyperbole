{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Test where

import Data.Aeson
import Data.Text (Text)
import Debug.Trace
import Example.AppRoute
import Example.Colors
import Example.Effects.Debug
import Example.Style.Cyber (btn)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es) => Page es '[Message, Other, Fake]
page = do
  -- trigger Fake Noop
  pure $ exampleLayout Test $ do
    script "test.js"
    example "Test" "Example/Page/Test.hs" $ do
      col ~ embed $ do
        hyper Message1 $ messageView "Hello"
        hyper Message2 $ messageView "World!"
        hyper Other $ button GoTrigger ~ btn $ "Go Trigger"

data Message = Message1 | Message2
  deriving (Generic, ViewId, Show)

instance (Debug :> es) => HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder m) = do
    v :: Message <- viewId
    traceM $ "Updating " <> show v
    let new = m <> "!"
    delay 1000
    traceM $ " - done " <> show v
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  row ~ gap 10 $ do
    button (Louder m) ~ border 1 . pad 5 . whenLoading (borderColor Danger . color Danger) $ "Louder"
    el ~ pad 5 $ text m

data Other = Other
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView Other es where
  type Require Other = '[Message, Fake]

  data Action Other
    = GoTrigger
    | Sneaky
    deriving (Generic, ViewAction)

  update GoTrigger = do
    trigger Fake Noop
    trigger Message2 (Louder "remote")
    pushEvent "hello" (String "woot")
    pure "OK"
  update Sneaky = do
    pure "Sneaky"

---- NOT ON PAGE ---------------------------

data Fake = Fake
  deriving (Generic, ViewId)

instance HyperView Fake es where
  data Action Fake
    = Noop
    deriving (Generic, ViewAction)

  update _ = do
    pure "OK"
