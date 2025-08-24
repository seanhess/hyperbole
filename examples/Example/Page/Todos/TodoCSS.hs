{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.TodoCSS (page) where

import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Text qualified as T
import Example.Effects.Todos (FilterTodo (..), Todo, TodoId, Todos)
import Example.Effects.Todos qualified as Todos
import Example.Page.Todos.Todo (Action (..), AllTodos (..), TodoForm (..), TodoView (..), pluralize)
import Web.Hyperbole as Hyperbole
import Web.Hyperbole.HyperView.Forms (Input (Input))

{-

To make the CSS version work and overcome the default CSS reset, we tweaked the output slightly via a few style tags here and there:

only need to add one manual rule to the footer, to override the CSS reset

- main title
  - override its absolute positioning
- read-only item:
  - restore border-bottom (a visual separator)
- first footer
  - add bottom padding
- second footer
  - restore default user-agent p tags margin

-}

page :: (Todos :> es) => Page es '[CSSTodos, CSSTodo]
page = do
  todos <- Todos.loadAll
  pure $ do
    div' $ do
      -- Alternative stylesheet at: https://todomvc.com/examples/javascript-es6/dist/app.css
      -- Reference implementation at: https://todomvc.com/examples/javascript-es6/dist/
      stylesheet "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"

      -- Tweaks required to the stylesheet, mostly to undo the global reset we used for the
      -- rest of the examples, but also to accomodate a slightly different DOM
      stylesheet "/todomvc.css"

      section @ class_ "todoapp" $ do
        hyper CSSTodos $ todosView FilterAll todos

      footer @ class_ "info" $ do
        p "Double-click to edit a todo"
        p $ do
          span' "Go back to the "
          a @ att "href" "/examples" $ "examples"

--- TodosView ----------------------------------------------------------------------------

data CSSTodos = CSSTodos
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView CSSTodos es where
  type Require CSSTodos = '[CSSTodo]

  -- reuse as the actions from the main TodoMVC example. This isn't a good
  -- example of how to factor well, it's optimized to make the main example
  -- readable. Focus on the views
  newtype Action CSSTodos = MkTodosAction (Action AllTodos)
    deriving newtype (ViewAction)

  -- Repeated logic from the main Todos example. Do not follow this as an example
  -- of how to reuse views
  update (MkTodosAction action) = do
    case action of
      ClearCompleted -> do
        todosView FilterAll <$> Todos.clearCompleted
      SubmitTodo -> do
        TodoForm task <- formData @(TodoForm Identity)
        _ <- Todos.create task
        todos <- Todos.filteredTodos FilterAll
        pure $ todosView FilterAll todos
      Filter filt -> do
        todos <- Todos.filteredTodos filt
        pure $ todosView filt todos
      ToggleAll filt -> do
        todos <- Todos.filteredTodos filt >>= Todos.toggleAll
        pure $ todosView filt todos
      SetCompleted filt todo completed -> do
        _ <- Todos.setCompleted completed todo
        todos <- Todos.filteredTodos filt
        pure $ todosView filt todos
      Destroy filt todo -> do
        Todos.clear todo
        todos <- Todos.filteredTodos filt
        pure $ todosView filt todos

todosView :: FilterTodo -> [Todo] -> View CSSTodos ()
todosView filt todos = do
  header @ class_ "header" $ do
    h1 $ text "todos"
    todoForm
  main' @ class_ "main" $ do
    div' @ class_ "toggle-all-container" $ do
      input'
        @ class_ "toggle-all"
        . att "id" "toggle-all"
        . att "type" "checkbox"

      label'
        @ class_ "toggle-all-label"
        . att "for" "toggle-all"
        . onClick (MkTodosAction $ ToggleAll filt)
        $ text "Mark all as complete"

      ul' @ class_ "todo-list" $ do
        forM_ todos $ \todo -> do
          hyper (CSSTodo todo.id) $ todoView filt todo

    statusBar filt todos

todoForm :: View CSSTodos ()
todoForm = do
  let f :: TodoForm FieldName = fieldNames
  form (MkTodosAction SubmitTodo) $ do
    field f.task $ label $ do
      Input (FieldName nm) <- context
      input' -- we use a custom input field, because the Hyperbole one overrides autocomplete
        @ class_ "new-todo"
        {-
          -- . autofocus
          FIXME: turning off autofocus, that "steals" the focus on item click.
          FIXME: to solve this, we could either store the "initially focused" state and track that boolean, or use buttons
          FIXME: but since this example is meant to match as close as possible to the original CSS version
          FIXME: and not diverge too much from the other todo example, I'm leaving as-is.
         -}
        . att "autocomplete" "off"
        . placeholder "What needs to be done?"
        . value ""
        . name nm -- because we use a custom field, we must provide this param for the library

statusBar :: FilterTodo -> [Todo] -> View CSSTodos ()
statusBar filt todos = do
  footer @ class_ "footer" $ do
    let numLeft = length $ filter (\t -> not t.completed) todos
    span' @ class_ "todo-count" $ do
      text $
        mconcat
          [ T.pack $ show numLeft
          , " "
          , pluralize numLeft "item" "items"
          , " "
          , "left!"
          ]
    space
    ul' @ class_ "filters" $ do
      filterLi FilterAll "All"
      filterLi Active "Active"
      filterLi Completed "Completed"
    space
    button (MkTodosAction ClearCompleted) @ class_ "clear-completed" $ "Clear completed"
 where
  filterLi f str =
    li' @ class_ "filter" . selectedFilter f $ do
      a
        @ onClick (MkTodosAction $ Filter f)
        . att "href" "" -- harmless empty href is for the CSS
        $ text str
  selectedFilter f =
    if f == filt then class_ "selected" else id

--- TodoView ----------------------------------------------------------------------------

data CSSTodo = CSSTodo TodoId
  deriving (Generic, ViewId)

instance (Todos :> es) => HyperView CSSTodo es where
  type Require CSSTodo = '[CSSTodos]

  newtype Action CSSTodo
    = MkTodoAction (Action TodoView)
    deriving newtype (ViewAction)

  update (MkTodoAction action) =
    case action of
      Edit filt todo -> do
        pure $ todoEditView filt todo
      SubmitEdit filt todo -> do
        TodoForm task <- formData @(TodoForm Identity)
        t <- Todos.setTask task todo
        pure $ todoView filt t

todoView :: FilterTodo -> Todo -> View CSSTodo ()
todoView filt todo = do
  li'
    @ bool id (class_ "completed") todo.completed
    $ do
      div' @ class_ "view" $ do
        target CSSTodos $ do
          input'
            @ class_ "toggle"
            . att "type" "checkbox"
            . onClick (MkTodosAction $ SetCompleted filt todo $ not todo.completed)
            . checked todo.completed

        label' @ class_ "label" . onDblClick (MkTodoAction $ Edit filt todo) $ do
          text todo.task

        target CSSTodos $ do
          button (MkTodosAction $ Destroy filt todo) @ class_ "destroy" $ ""

todoEditView :: FilterTodo -> Todo -> View CSSTodo ()
todoEditView filt todo = do
  li' @ class_ "editing" $ do
    form (MkTodoAction $ SubmitEdit filt todo) $ do
      field "task" $ label $ do
        input TextInput
          @ class_ "edit"
          . value todo.task
          . autofocus

--- Semantic HTML Helpers ----------------------------------------------------------------------------
--
-- you can use semantic HTML with atomic-css too! But it is required here for the stylesheet to work

div' :: View c () -> View c ()
div' = tag "div"

span' :: View c () -> View c ()
span' = tag "span"

section :: View c () -> View c ()
section = tag "section"

header :: View c () -> View c ()
header = tag "header"

main' :: View c () -> View c ()
main' = tag "main"

h1 :: View c () -> View c ()
h1 = tag "h1"

p :: View c () -> View c ()
p = tag "p"

label' :: View c () -> View c ()
label' = tag "label"

input' :: View c ()
input' = tag "input" none

a :: View c () -> View c ()
a = tag "a"

ul' :: View c () -> View c ()
ul' = tag "ul"

li' :: View c () -> View c ()
li' = tag "li"

footer :: View c () -> View c ()
footer = tag "footer"
