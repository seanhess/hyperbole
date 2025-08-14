{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.Simple where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Types.Event

main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)

data Prefs = Prefs {msg :: Text}
  deriving (Generic, ToQuery)

page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  trigger Message2 (Louder "Whatever")
  pushEvent "hello" ()
  setQuery $ Prefs "HI" -- ignored! not ideal...
  pure $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

data Message = Message1 | Message2
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pushEvent "hello2" ()
    pure $ messageView new

messageView :: Text -> View Message ()
messageView msg = do
  button (Louder msg) ~ border 1 $ text msg
