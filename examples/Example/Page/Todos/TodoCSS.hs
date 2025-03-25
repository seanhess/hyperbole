{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.TodoCSS (page) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Effectful
import Example.Colors (AppColor (..))
import Example.Effects.Todos (Todo, TodoId, Todos)
import Example.Effects.Todos qualified as Todos
import Example.Page.Todos.Shared
import Example.Style qualified as Style
import Example.View.Icon qualified as Icon
import Example.View.Inputs (toggleCheckbox)
import Web.Hyperbole as Hyperbole

page :: (Todos :> es) => Eff es (Page '[TodosView, TodoView])
page = do
  todos <- Todos.loadAll
  pure $ tag "div" id $ do
    tag "h1" id $ text "Todos CSS"
    tag "p" id $
      text ("Count:" <> T.pack (show $ length todos))
    hyper MkTodosView $ todosView FilterAll todos

--- TodosView ----------------------------------------------------------------------------

data TodosView = MkTodosView
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView TodosView es where
  type Require TodosView = '[TodoView]

  data Action TodosView
    = ClearCompleted
    | Filter FilterTodo
    | SubmitTodo
    | ToggleAll FilterTodo
    | SetCompleted FilterTodo Todo Bool
    deriving (Generic, ViewAction)

  update = \case
    SubmitTodo -> do
      TodoForm task <- formData @(TodoForm Identity)
      _ <- Todos.create task
      todos <- Todos.loadAll
      pure $ todosView FilterAll todos
    ToggleAll filt -> do
      todos <- filteredTodos filt
      updated <- Todos.toggleAll todos
      pure $ todosView filt updated
    ClearCompleted -> do
      todos <- Todos.clearCompleted
      pure $ todosView FilterAll todos
    Filter filt -> do
      todos <- filteredTodos filt
      pure $ todosView filt todos
    SetCompleted filt todo completed -> do
      _ <- Todos.setCompleted completed todo
      todos <- filteredTodos filt
      pure $ todosView filt todos
   where
    filteredTodos filt =
      filter (isFilter filt) <$> Todos.loadAll

    isFilter filt todo =
      case filt of
        FilterAll -> True
        Active -> not todo.completed
        Completed -> todo.completed

todosView :: FilterTodo -> [Todo] -> View TodosView ()
todosView filt todos = do
  todoForm filt
  col id $ do
    forM_ todos $ \todo -> do
      hyper (MkTodoView todo.id) $ todoView filt todo
  statusBar filt todos

todoForm :: FilterTodo -> View TodosView ()
todoForm filt = do
  let f :: TodoForm FieldName = fieldNames
  tag "div" id $ do
    tag "span" id $ do
      button (ToggleAll filt) (width 32 . hover (color Primary)) Icon.chevronDown
    form SubmitTodo grow $ do
      field f.task id $ do
        input TextInput (pad 12 . placeholder "What needs to be done?" . value "")

statusBar :: FilterTodo -> [Todo] -> View TodosView ()
statusBar filt todos = do
  tag "div" id $ do
    let numLeft = length $ filter (\t -> not t.completed) todos
    el_ $ do
      text $ T.pack (show numLeft)
      text " items left!"
    space
    tag "div" id $ do
      filterButton FilterAll "All"
      filterButton Active "Active"
      filterButton Completed "Completed"
    space
    button ClearCompleted (hover (color Primary)) "Clear completed"
 where
  filterButton f =
    button (Filter f) (selectedFilter f . pad (XY 4 0) . rounded 2)
  selectedFilter f =
    if f == filt then border 1 else id

--- TodoView ----------------------------------------------------------------------------

data TodoView = MkTodoView TodoId
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView TodoView es where
  type Require TodoView = '[TodosView]

  data Action TodoView
    = Edit FilterTodo Todo
    | SubmitEdit FilterTodo Todo
    deriving (Generic, ViewAction)

  update (Edit filt todo) = do
    pure $ todoEditView filt todo
  update (SubmitEdit filt todo) = do
    TodoForm task <- formData @(TodoForm Identity)
    updated <- Todos.setTask task todo
    pure $ todoView filt updated

todoView :: FilterTodo -> Todo -> View TodoView ()
todoView filt todo = do
  tag "div" id $ do
    target MkTodosView $ do
      toggleCheckbox (SetCompleted filt todo) todo.completed
    tag "span" (completed . pad (XY 18 4) . onDblClick (Edit filt todo)) $ text todo.task
 where
  completed = if todo.completed then Style.strikethrough else id

todoEditView :: FilterTodo -> Todo -> View TodoView ()
todoEditView filt todo = do
  let f = fieldNames @TodoForm
  tag "div" id $ do
    form (SubmitEdit filt todo) (pad (TRBL 0 0 0 46)) $ do
      field f.task id $ do
        input TextInput (pad 4 . value todo.task . autofocus)
