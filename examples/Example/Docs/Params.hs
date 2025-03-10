module Example.Docs.Params where

import Data.Text (Text)
import Web.Hyperbole

data Filters = Filters
  { search :: Text
  }
  deriving (ToQuery, FromQuery, Generic)

page :: (Hyperbole :> es) => Eff es (Page '[Todos])
page = do
  filters <- query @Filters
  todos <- loadTodos filters
  pure $ do
    hyper Todos $ todosView todos

data Todos = Todos
  deriving (Generic, ViewId)

instance HyperView Todos es where
  data Action Todos
    = SetSearch Text
    deriving (Generic, ViewAction)

  update (SetSearch term) = do
    let filters = Filters term
    setQuery filters
    todos <- loadTodos filters
    pure $ todosView todos

-- Fake User effect
data Todo

loadTodos :: Filters -> Eff es [Todo]
loadTodos _ = pure []

-- Fake Todo View
todosView :: [Todo] -> View Todos ()
todosView _ = none

page' :: (Hyperbole :> es) => Eff es (Page '[Message])
page' = do
  msg <- param "message"
  pure $ do
    hyper Message $ messageView msg

messageView :: Text -> View Message ()
messageView m = do
  el bold $ text $ "Message: " <> m
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage msg) = do
    setParam "message" msg
    pure $ messageView msg
