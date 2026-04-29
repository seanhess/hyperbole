{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Effects.Database where

import App.Docs
import App.Route qualified as Route
import Database.Selda
import Database.Selda.Backend.Internal (SeldaConnection, runSeldaT)
import Database.Selda.SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Example.Effects.Todos (FilterTodo (..), Todo (..), Todos (..), randomId)
import Example.Todos.Todo (AllTodos (..), TodoView (..), todosView)
import Example.View.Layout
import Web.Hyperbole hiding (Table, query, select, table)

type DB = SeldaConnection SQLite

-----------------------------------------------------------------------
-- Database
-----------------------------------------------------------------------

-- Selda is a EDSL that supports both SQLite and PostgreSQL
-- See https://valderman.github.io/selda/
runTodosSelda
  :: forall es a
   . (IOE :> es)
  => SeldaConnection SQLite
  -> Eff (Todos : es) a
  -> Eff es a
runTodosSelda conn = reinterpret (runReader conn) $ \_ -> \case
  LoadAll -> runSelda $ query $ do
    select todos
  Save todo -> do
    runSelda $ update_ todos (isTodo todo.id) $ \r ->
      r
        `with` [ #task := literal todo.task
               , #completed := literal todo.completed
               ]
  Remove todoId -> do
    _ <- runSelda $ deleteFrom todos (isTodo todoId)
    pure ()
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    runSelda $ insert_ todos [todo]
    pure todoId
 where
  isTodo todoId r = r ! #id .== literal todoId

todos :: Table Todo
todos = table "todos" [#id :- primary]

initTodosDatabase :: (MonadIO m, MonadMask m) => m (SeldaConnection SQLite)
initTodosDatabase = do
  conn <- sqliteOpen "todos.sqlite"
  _ <- liftIO $ runEff . runReader conn . runSelda $ tryCreateTable todos
  pure conn

runSelda :: (Reader (SeldaConnection SQLite) :> es, IOE :> es) => SeldaT SQLite (Eff es) a -> Eff es a
runSelda m = do
  conn <- ask
  runSeldaT m conn

-- Documentation ---------------------------------------------

main :: IO ()
main = do
  db <- initTodosDatabase
  run 3000 $ do
    liveApp quickStartDocument (runTodosSelda db $ runPage page)

page :: (Todos :> es) => Page es '[AllTodos, TodoView]
page = do
  ts <- send LoadAll
  pure $ layout (Route.Examples Route.TodosDB) $ do
    section' "Todos Database" $ do
      example $(moduleSource) $ do
        hyper AllTodos $ todosView FilterAll ts
