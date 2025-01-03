{-# LANGUAGE LambdaCase #-}

module Example.Effects.Todos where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (randomRIO)
import Web.Hyperbole (FromQueryData (..), Hyperbole, ToQueryData (..), clearSession, session, setSession)

type TodoId = Text

data Todo = Todo
  { id :: TodoId
  , task :: Text
  , completed :: Bool
  }
  deriving (Show, Read, ToQueryData, FromQueryData)

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
    ids <- sessionTodoIds
    catMaybes <$> mapM session ids
  Save todo -> do
    setSession todo.id todo
  Remove todoId -> do
    ids <- sessionTodoIds
    sessionSaveTodoIds $ filter (/= todoId) ids
    clearSession todoId
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    ids <- sessionTodoIds
    setSession todo.id todo
    sessionSaveTodoIds (todo.id : ids)
    pure todoId
 where
  randomId :: (IOE :> es) => Eff es Text
  randomId = pack . show <$> randomRIO @Int (0, 9999999)

  sessionTodoIds :: (Hyperbole :> es) => Eff es [TodoId]
  sessionTodoIds = do
    fromMaybe mempty <$> session "todoIds"

  sessionSaveTodoIds :: (Hyperbole :> es) => [TodoId] -> Eff es ()
  sessionSaveTodoIds = setSession "todoIds"

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
