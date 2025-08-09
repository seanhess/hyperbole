module Example.Page.Todos.Shared
  ( pluralize
  , FilterTodo (..)
  , TodoForm (..)
  ) where

import Data.Text (Text)
import Web.Hyperbole

data FilterTodo
  = FilterAll
  | Active
  | Completed
  deriving (Eq, Generic, ToJSON, FromJSON)

data TodoForm f = TodoForm
  { task :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName)

pluralize :: Int -> Text -> Text -> Text
pluralize n singular plural =
  if n == 1
    then
      singular
    else
      plural
