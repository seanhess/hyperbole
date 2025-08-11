{-# LANGUAGE LambdaCase #-}

module Example.Page.Todos.Shared
  ( pluralize
  , FilterTodo (..)
  , TodoForm (..)
  , AllTodos (..)
  , TodosAction (..)
  , updateTodos
  , TodoAction (..)
  , updateTodo
  ) where

import Data.Text (Text)
import Effectful
import Example.Effects.Todos (Todo (..), Todos)
import Example.Effects.Todos qualified as Todos
import Web.Hyperbole as Hyperbole hiding (Action, update)

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

data AllTodos = AllTodos
  deriving (Generic, ViewId)

data TodosAction
  = ClearCompleted
  | Filter FilterTodo
  | SubmitTodo
  | ToggleAll FilterTodo
  | SetCompleted FilterTodo Todo Bool
  | Destroy FilterTodo Todo
  deriving (Generic, ViewAction)

updateTodos :: (Todos :> es, Hyperbole :> es) => TodosAction -> Eff es [Todo]
updateTodos = \case
  SubmitTodo -> do
    TodoForm task <- formData @(TodoForm Identity)
    _ <- Todos.create task
    Todos.loadAll
  ToggleAll filt -> do
    todos <- filteredTodos filt
    Todos.toggleAll todos
  ClearCompleted -> do
    Todos.clearCompleted
  Filter filt -> do
    filteredTodos filt
  SetCompleted filt todo completed -> do
    _ <- Todos.setCompleted completed todo
    filteredTodos filt
  Destroy filt todo -> do
    Todos.clear todo
    filteredTodos filt
 where
  filteredTodos filt =
    filter (isFilter filt) <$> Todos.loadAll

  isFilter filt todo =
    case filt of
      FilterAll -> True
      Active -> not todo.completed
      Completed -> todo.completed

data TodoAction
  = SubmitEdit Todo
  deriving (Generic, ViewAction, ToJSON, FromJSON)

updateTodo :: (Todos :> es, Hyperbole :> es) => TodoAction -> Eff es Todo
updateTodo = \case
  SubmitEdit todo -> do
    TodoForm task <- formData @(TodoForm Identity)
    Todos.setTask task todo
