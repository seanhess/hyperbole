{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todo where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Todos (Todo (..), TodoId, Todos, runTodosSession)
import Example.Effects.Todos qualified as Todos
import Example.Style qualified as Style
import Example.View.Icon qualified as Icon
import Example.View.Inputs (toggleCheckBtn)
import Example.View.Layout (exampleLayout)
import Web.Hyperbole as Hyperbole

page :: (Todos :> es) => Eff es (Page '[AllTodos, TodoView])
page = do
  todos <- Todos.loadAll
  pure $ exampleLayout Route.Todos $ do
    col (gap 10 . grow) $ do
      row id $ do
        space
        el (Style.h1 . pad 10) "Todos"
        space
      hyper AllTodos $ todosView FilterAll todos

simplePage :: (Todos :> es) => Eff es (Page '[AllTodos, TodoView])
simplePage = do
  todos <- Todos.loadAll
  pure $ do
    hyper AllTodos $ todosView FilterAll todos

--- AllTodos ----------------------------------------------------------------------------

data AllTodos = AllTodos
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView AllTodos es where
  type Require AllTodos = '[TodoView]

  data Action AllTodos
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

data FilterTodo
  = FilterAll
  | Active
  | Completed
  deriving (Eq, Generic, ToJSON, FromJSON)

todosView :: FilterTodo -> [Todo] -> View AllTodos ()
todosView filt todos = do
  todoForm filt
  col id $ do
    forM_ todos $ \todo -> do
      hyper (TodoView todo.id) $ todoView filt todo
  statusBar filt todos

todoForm :: FilterTodo -> View AllTodos ()
todoForm filt = do
  let f :: TodoForm FieldName = fieldNames
  row (border 1) $ do
    el (pad 8) $ do
      button (ToggleAll filt) (width 32 . hover (color Primary)) Icon.chevronDown
    form SubmitTodo grow $ do
      field f.task id $ do
        input TextInput (pad 12 . placeholder "What needs to be done?" . value "")

data TodoForm f = TodoForm
  { task :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName)

statusBar :: FilterTodo -> [Todo] -> View AllTodos ()
statusBar filt todos = do
  row (pad 10 . color SecondaryLight) $ do
    let numLeft = length $ filter (\t -> not t.completed) todos
    el_ $ do
      text $ pack (show numLeft)
      text " items left!"
    space
    row (gap 10) $ do
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

data TodoView = TodoView TodoId
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView TodoView es where
  type Require TodoView = '[AllTodos]

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
  row (border (TRBL 0 0 1 0) . pad 10) $ do
    target AllTodos $ do
      toggleCheckBtn (SetCompleted filt todo) todo.completed
    el (completed . pad (XY 18 4) . onDblClick (Edit filt todo)) $ text todo.task
 where
  completed = if todo.completed then Style.strikethrough else id

todoEditView :: FilterTodo -> Todo -> View TodoView ()
todoEditView filt todo = do
  let f = fieldNames @TodoForm
  row (border (TRBL 0 0 1 0) . pad 10) $ do
    form (SubmitEdit filt todo) (pad (TRBL 0 0 0 46)) $ do
      field f.task id $ do
        input TextInput (pad 4 . value todo.task . autofocus)

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runTodosSession $ runPage page)

