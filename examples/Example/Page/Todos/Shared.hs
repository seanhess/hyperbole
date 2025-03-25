{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.Shared where

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
