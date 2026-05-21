{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Effects.Database where

import App.Docs
import App.Route qualified as Route
import Database.SQLite.Simple as SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Effects.Todos (FilterTodo (..), Todo (..), Todos (..), randomId)
import Example.Todos.Todo (AllTodos (..), TodoView (..), todosView)
import Example.View.Layout
import Web.Hyperbole hiding (Table, query, select, table)

-----------------------------------------------------------------------
-- Database
-----------------------------------------------------------------------

type DB = SQLite.Connection

runTodosSQLite
  :: forall es a
   . (IOE :> es)
  => DB
  -> Eff (Todos : es) a
  -> Eff es a
runTodosSQLite conn = interpret $ \_ -> \case
  LoadAll ->
    liftIO $ query_ conn "SELECT * from todos"
  Save todo -> do
    liftIO $ execute conn "UPDATE todos SET task = ?, completed = ? WHERE id = ?" (todo.task, todo.completed, todo.id)
  Remove todoId ->
    liftIO $ execute conn "DELETE FROM todos WHERE id = ?" (Only todoId)
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    liftIO $ execute conn "INSERT INTO todos (id, task, completed) VALUES (?,?,?)" todo
    pure todoId

initTodosDatabase :: (MonadIO m) => m DB
initTodosDatabase = liftIO $ do
  conn <- SQLite.open "todos.sqlite"
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (id TEXT PRIMARY KEY, task TEXT, completed BOOL)"
  pure conn

-- Documentation ---------------------------------------------

main :: IO ()
main = do
  db <- initTodosDatabase
  run 3000 $ do
    liveApp quickStartDocument (runTodosSQLite db $ runPage page)

page :: (Todos :> es) => Page es '[AllTodos, TodoView]
page = do
  ts <- send LoadAll
  pure $ layout (Route.Examples Route.TodosDB) $ do
    section' "Todos Database" $ do
      example $(moduleSource) $ do
        hyper AllTodos $ todosView FilterAll ts
