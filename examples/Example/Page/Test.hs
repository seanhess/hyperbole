{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Test where

import Data.Text (Text)
import Debug.Trace
import Example.AppRoute
import Example.Colors
import Example.Effects.Debug
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Message])
page = do
  pure $ exampleLayout Test $ do
    example "Test" "Example/Page/Test.hs" $ do
      col ~ embed $ do
        hyper Message1 $ messageView "Hello"
        hyper Message2 $ messageView "World!"

data Message = Message1 | Message2
  deriving (Generic, ViewId, Show)

instance (Debug :> es) => HyperView Message es where
  data Action Message = Louder Text
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
