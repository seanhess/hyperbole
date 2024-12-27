module Example.Intro.Nested where

import Control.Monad (forM_)
import Data.Text (Text)
import Web.Hyperbole


page :: (Hyperbole :> es) => Eff es (Page '[AllTodos, TodoItem])
page = do
  pure $ do
    hyper AllTodos $ todosView allTodos
 where
  allTodos = [todo "One", todo "Two", todo " Three"]
  todo t = Todo t False


data Todo = Todo
  { task :: Text
  , completed :: Bool
  }
  deriving (Show, Read, Eq)


data AllTodos = AllTodos
  deriving (Show, Read, ViewId)


instance HyperView AllTodos es where
  type Require AllTodos = '[TodoItem]


  data Action AllTodos
    = AddTodo Text [Todo]
    deriving (Show, Read, ViewAction)


  update (AddTodo txt todos) = do
    let new = Todo txt False : todos
    pure $ todosView new


todosView :: [Todo] -> View AllTodos ()
todosView todos = do
  forM_ todos $ \todo -> do
    hyper TodoItem $ todoView todo
  button (AddTodo "Shopping" todos) id "Add Shopping"


data TodoItem = TodoItem
  deriving (Show, Read, ViewId)


instance HyperView TodoItem es where
  data Action TodoItem
    = Complete Todo
    deriving (Show, Read, ViewAction)


  update (Complete todo) = do
    let new = todo{completed = True}
    pure $ todoView new


todoView :: Todo -> View TodoItem ()
todoView todo = do
  el_ (text todo.task)
  button (Complete todo) id "Mark Completed"
