module Example.Docs.SideEffects where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Message]
page = do
  prm <- lookupParam "msg"
  let msg = fromMaybe "hello" prm
  pure $ do
    hyper Message $ messageView msg

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    setParam "msg" new
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) ~ border 1 $ "Louder"
  el ~ bold $ text $ "Message: " <> m
