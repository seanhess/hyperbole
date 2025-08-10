{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.Todo where

import Control.Monad (forM_)
import Data.Text (pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Todos (Todo (..), TodoId, Todos, runTodosSession)
import Example.Effects.Todos qualified as Todos
import Example.Page.Todos.Shared (FilterTodo (Active, Completed, FilterAll), TodoForm (..))
import Example.Page.Todos.Shared qualified as Shared
import Example.Style qualified as Style
import Example.View.Icon qualified as Icon
import Example.View.Inputs (toggleCheckbox)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole

page :: (Todos :> es) => Eff es (Page '[AllTodos, TodoView])
page = do
  todos <- Todos.loadAll
  pure $ exampleLayout (Route.Examples Route.Todos) $ do
    example "Todos" "Example/Page/Todo.hs" $ do
      col ~ embed $ hyper AllTodos $ todosView FilterAll todos

-- Keep this, it's used for documentation (+ usable via the REPL, see main below)
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

  newtype Action AllTodos = MkAction Shared.TodosAction
    deriving newtype (Generic, ViewAction)

  update (MkAction action) = do
    case action of
      Shared.ClearCompleted ->
        todosView FilterAll <$> Shared.updateTodos Shared.ClearCompleted
      Shared.SubmitTodo ->
        todosView FilterAll <$> Shared.updateTodos Shared.SubmitTodo
      Shared.Filter f ->
        todosView f <$> Shared.updateTodos (Shared.Filter f)
      Shared.ToggleAll f ->
        todosView f <$> Shared.updateTodos (Shared.ToggleAll f)
      Shared.SetCompleted f t b ->
        todosView f <$> Shared.updateTodos (Shared.SetCompleted f t b)

todosView :: FilterTodo -> [Todo] -> View AllTodos ()
todosView filt todos = do
  todoForm filt
  col $ do
    forM_ todos $ \todo -> do
      hyper (TodoView todo.id) $ todoView filt todo
  statusBar filt todos

todoForm :: FilterTodo -> View AllTodos ()
todoForm filt = do
  let f :: TodoForm FieldName = fieldNames
  row ~ border 1 $ do
    el ~ pad 8 $ do
      button (MkAction $ Shared.ToggleAll filt) Icon.chevronDown ~ width 32 . hover (color Primary)
    form (MkAction Shared.SubmitTodo) ~ grow $ do
      field f.task $ do
        input TextInput ~ pad 12 @ placeholder "What needs to be done?" . value ""

statusBar :: FilterTodo -> [Todo] -> View AllTodos ()
statusBar filt todos = do
  row ~ pad 10 . color SecondaryLight $ do
    let numLeft = length $ filter (\t -> not t.completed) todos
    el $ do
      text $ pack (show numLeft)
      text " items left!"
    space
    row ~ gap 10 $ do
      filterButton FilterAll "All"
      filterButton Active "Active"
      filterButton Completed "Completed"
    space
    button (MkAction Shared.ClearCompleted) ~ hover (color Primary) $ "Clear completed"
 where
  filterButton f =
    button (MkAction $ Shared.Filter f) ~ selectedFilter f . pad (XY 4 0) . rounded 2
  selectedFilter f =
    if f == filt then border 1 else id

--- TodoView ----------------------------------------------------------------------------

data TodoView = TodoView TodoId
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView TodoView es where
  type Require TodoView = '[AllTodos]

  data Action TodoView
    = Edit FilterTodo Todo
    | MkTodoViewAction FilterTodo Shared.TodoAction
    deriving (Generic, ViewAction)

  update (Edit filt todo) = do
    pure $ todoEditView filt todo
  update (MkTodoViewAction filt action) =
    todoView filt <$> Shared.updateTodo action

todoView :: FilterTodo -> Todo -> View TodoView ()
todoView filt todo = do
  row ~ border (TRBL 0 0 1 0) . pad 10 $ do
    target AllTodos $ do
      toggleCheckbox (MkAction . Shared.SetCompleted filt todo) todo.completed
    el (text todo.task) @ onDblClick (Edit filt todo) ~ completed . pad (XY 18 4)
 where
  completed = if todo.completed then Style.strikethrough else id

todoEditView :: FilterTodo -> Todo -> View TodoView ()
todoEditView filt todo = do
  let f = fieldNames @TodoForm
  row ~ border (TRBL 0 0 1 0) . pad 10 $ do
    form (MkTodoViewAction filt $ Shared.SubmitEdit todo) ~ pad (TRBL 0 0 0 46) $ do
      field f.task $ do
        input TextInput @ value todo.task . autofocus ~ pad 4

{-
You may try this in the REPL for simple tests:

bash> cabal repl exe:examples lib:hyperbole
ghci> Todo.main
-}
main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Todo (simple)") (runTodosSession $ runPage simplePage)
