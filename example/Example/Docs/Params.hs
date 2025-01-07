module Example.Docs.Params where

import Data.Text (Text)
import Web.Hyperbole

data Filters = Filters
  { search :: Text
  }
  deriving (Generic, Show, Read, ToQuery, FromQuery)

page :: (Hyperbole :> es) => Eff es (Page '[Todos])
page = do
  filters <- query @Filters
  todos <- loadTodos filters
  pure $ do
    hyper Todos $ todosView todos

data Todos = Todos
  deriving (Show, Read, ViewId)

instance HyperView Todos es where
  data Action Todos
    = SetFilters Filters
    deriving (Show, Read, ViewAction)

  update (SetFilters f) = do
    setQuery f
    todos <- loadTodos f
    pure $ todosView todos

-- Fake User effect
data Todo

loadTodos :: Filters -> Eff es [Todo]
loadTodos _ = pure []

-- Fake Todo View
todosView :: [Todo] -> View Todos ()
todosView _ = none

messagePage :: (Hyperbole :> es) => Eff es (Page '[Message])
messagePage = do
  msg <- param "message"
  pure $ do
    hyper Message $ messageView msg

messageView :: Text -> View Message ()
messageView m = do
  el bold $ text $ "Message: " <> m
  button (SetMessage "Goodbye") (border 1) "Say Goodbye"

data Message = Message
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Show, Read, ViewAction)

  update (SetMessage msg) = do
    setParam "message" msg
    pure $ messageView msg
