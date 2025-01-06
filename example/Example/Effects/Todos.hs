{-# LANGUAGE LambdaCase #-}

module Example.Effects.Todos where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (randomRIO)
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData qualified as QueryData

type TodoId = Text

newtype AllTodos = AllTodos [Todo]

instance ToQuery AllTodos where
  toQuery (AllTodos todos) =
    foldr (\todo -> QueryData.insert todo.id todo) mempty todos

instance FromQuery AllTodos where
  parseQuery qd = do
    let qd' = QueryData.filterKey isTodoItem qd
    todos <- mapM parseParam $ QueryData.elems qd'
    pure $ AllTodos todos
   where
    isTodoItem k = "todo-" `T.isPrefixOf` k

data Todo = Todo
  { id :: TodoId
  , task :: Text
  , completed :: Bool
  }
  deriving (Show, Read, ToParam, FromParam)

data Todos :: Effect where
  LoadAll :: Todos m [Todo]
  Save :: Todo -> Todos m ()
  Remove :: TodoId -> Todos m ()
  Create :: Text -> Todos m TodoId
type instance DispatchOf Todos = 'Dynamic
runTodosSession
  :: forall es a
   . (Hyperbole :> es, IOE :> es)
  => Eff (Todos : es) a
  -> Eff es a
runTodosSession = interpret $ \_ -> \case
  LoadAll -> do
    AllTodos todos <- session
    pure todos
  Save todo -> do
    setSessionKey todo.id todo
  Remove todoId -> do
    deleteSessionKey todoId
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    setSessionKey todo.id todo
    pure todoId
 where
  randomId :: (IOE :> es) => Eff es Text
  randomId = do
    n <- randomRIO @Int (0, 9999999)
    pure $ "todo-" <> pack (show n)

loadAll :: (Todos :> es) => Eff es [Todo]
loadAll = send LoadAll

create :: (Todos :> es) => Text -> Eff es TodoId
create t = send $ Create t

setTask :: (Todos :> es) => Text -> Todo -> Eff es Todo
setTask task t = do
  let updated = t{task}
  send $ Save updated
  pure updated

setCompleted :: (Todos :> es) => Bool -> Todo -> Eff es Todo
setCompleted completed todo = do
  let updated = todo{completed}
  send $ Save updated
  pure updated

toggleAll :: (Todos :> es) => [Todo] -> Eff es [Todo]
toggleAll todos = do
  let shouldComplete = any (\t -> not t.completed) todos
  mapM (setCompleted shouldComplete) todos

clearCompleted :: (Todos :> es) => Eff es [Todo]
clearCompleted = do
  todos <- loadAll
  let completed = filter (.completed) todos
  mapM_ clear completed
  loadAll

clear :: (Todos :> es) => Todo -> Eff es ()
clear todo = do
  send $ Remove todo.id
