
module Example.Docs.SideEffects where

import Data.Text (Text)
import Web.Hyperbole
import Data.Default (Default(..))

data MessageSession = MessageSession { message :: Text }
  deriving (Show, Read, Generic, ToParam, FromParam, Session)
instance Default MessageSession where
  def = MessageSession mempty

messagePage :: (Hyperbole :> es) => Eff es (Page '[Message])
messagePage = do
  MessageSession msg <- session
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
    saveSession $ MessageSession new
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  button (Louder m) (border 1) "Louder"
  el bold $ text $ "Message: " <> m
