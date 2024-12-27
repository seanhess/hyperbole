{-# LANGUAGE LambdaCase #-}

module Example.Effects.Todos where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (randomRIO)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.Hyperbole (Hyperbole, clearSession, session, setSession)
import Web.Hyperbole.Effect.Session (readQueryParam, showQueryParam)


type TodoId = Text


data Todo = Todo
  { id :: TodoId
  , task :: Text
  , completed :: Bool
  }
  deriving (Show, Read)


newtype TodoIds = TodoIds [Text]
  deriving newtype (Show, Read, Monoid, Semigroup)


-- We need an instance of From/To HttpApiData to save to a session
instance FromHttpApiData Todo where
  parseQueryParam = readQueryParam
instance ToHttpApiData Todo where
  toQueryParam = showQueryParam


-- there's no list instance for some reason
instance FromHttpApiData TodoIds where
  parseQueryParam = readQueryParam
instance ToHttpApiData TodoIds where
  toQueryParam = showQueryParam


-- Load a user AND do next if missing?
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
    TodoIds ids <- sessionTodoIds
    catMaybes <$> mapM session ids
  Save todo -> do
    setSession todo.id todo
  Remove todoId -> do
    TodoIds ids <- sessionTodoIds
    sessionSaveTodoIds $ TodoIds $ filter (/= todoId) ids
    clearSession todoId
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    TodoIds ids <- sessionTodoIds
    setSession todo.id todo
    sessionSaveTodoIds $ TodoIds (todo.id : ids)
    pure todoId
 where
  randomId :: (IOE :> es) => Eff es Text
  randomId = pack . show <$> randomRIO @Int (0, 9999999)

  sessionTodoIds :: (Hyperbole :> es) => Eff es TodoIds
  sessionTodoIds = do
    fromMaybe mempty <$> session "todoIds"

  sessionSaveTodoIds :: (Hyperbole :> es) => TodoIds -> Eff es ()
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
