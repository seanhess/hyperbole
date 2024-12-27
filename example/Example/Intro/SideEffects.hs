module Example.Intro.SideEffects where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Web.Hyperbole

messagePage :: (Hyperbole :> es) => Eff es (Page '[Message])
messagePage = do
  stored <- session @Text "message"
  let msg = fromMaybe "Hello" stored
  pure $ do
    hyper Message $ messageView msg

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    setSession "message" new
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) (border 1) "Louder"
  el bold $ text $ "Message: " <> m
