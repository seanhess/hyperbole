{-# LANGUAGE LambdaCase #-}

module Example.Effects.Todos where

import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (randomRIO)
import Web.Hyperbole

type TodoId = Text

newtype AllTodos = AllTodos (Map TodoId Todo)
  deriving newtype (ToJSON, FromJSON)

instance Session AllTodos where
  sessionKey = "todos"
  cookiePath = Just "/examples" -- share data between both pages
instance Default AllTodos where
  def = AllTodos mempty

data Todo = Todo
  { id :: TodoId
  , task :: Text
  , completed :: Bool
  }
  deriving (Generic, ToJSON, FromJSON)

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
    pure $ M.elems todos
  Save todo -> do
    modifySession_ $ insert todo
  Remove todoId -> do
    modifySession_ $ delete todoId
  Create task -> do
    todoId <- randomId
    let todo = Todo todoId task False
    modifySession_ $ insert todo
    pure todoId
 where
  randomId :: (IOE :> es) => Eff es Text
  randomId = do
    n <- randomRIO @Int (0, 9999999)
    pure $ "todo-" <> pack (show n)

  insert :: Todo -> AllTodos -> AllTodos
  insert todo (AllTodos m) =
    AllTodos (M.insert todo.id todo m)

  delete :: TodoId -> AllTodos -> AllTodos
  delete todoId (AllTodos m) =
    AllTodos (M.delete todoId m)

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

filteredTodos :: (Todos :> es) => FilterTodo -> Eff es [Todo]
filteredTodos filt =
  filter (isFilter filt) <$> loadAll
 where
  isFilter f todo =
    case f of
      FilterAll -> True
      Active -> not todo.completed
      Completed -> todo.completed

data FilterTodo
  = FilterAll
  | Active
  | Completed
  deriving (Eq, Generic, ToJSON, FromJSON)
