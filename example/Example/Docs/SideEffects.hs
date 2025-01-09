
module Example.Docs.SideEffects where

import Data.Text (Text)
import Web.Hyperbole
import Data.Maybe (fromMaybe)


messagePage :: (Hyperbole :> es) => Eff es (Page '[Message])
messagePage = do
  prm <- lookupParam "msg"
  let msg = fromMaybe "hello" prm
  pure $ do
    hyper Message $ messageView msg

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    setParam "msg" new
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) (border 1) "Louder"
  el bold $ text $ "Message: " <> m