{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.TodoCSS (page) where

import Example.Effects.Todos (Todo, TodoId, Todos)

import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Text qualified as T
import Example.Effects.Todos qualified as Todos
import Example.Page.Todos.Shared (FilterTodo (..), TodoForm (..), pluralize)
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

page :: (Todos :> es) => Eff es (Page '[AllTodos, TodoView])
page = do
  todos <- Todos.loadAll
  pure $ do
    div' $ do
      -- Alternative stylesheet at: https://todomvc.com/examples/javascript-es6/dist/app.css
      -- Reference implementation at: https://todomvc.com/examples/javascript-es6/dist/
      stylesheet "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"
      section @ class_ "todoapp" $ do
        hyper AllTodos $ todosView FilterAll todos
      footer @ class_ "info" $ do
        let p_ = p @ style' "margin: 1em auto"
        p_ "Double-click to edit a todo"
        p_ $ do
          span' "Go back to the "
          a @ att "href" "/examples" . style' "color: #b83f45" $ "examples"

--- TodosView ----------------------------------------------------------------------------

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

todosView :: FilterTodo -> [Todo] -> View AllTodos ()
todosView filt todos = do
  header @ class_ "header" $ do
    h1 @ style' "top:-80px" $ text "todos"
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
        . onClick (ToggleAll filt)
        $ text "Mark all as complete"

      ul' @ class_ "todo-list" $ do
        forM_ todos $ \todo -> do
          hyper (TodoView todo.id) $ todoView filt todo

    statusBar filt todos

todoForm :: View AllTodos ()
todoForm = do
  let f :: TodoForm FieldName = fieldNames
  form SubmitTodo $ do
    field f.task $ do
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

statusBar :: FilterTodo -> [Todo] -> View AllTodos ()
statusBar filt todos = do
  footer @ class_ "footer" . style' "padding-bottom: 30px" $ do
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
    button ClearCompleted @ class_ "clear-completed" $ "Clear completed"
 where
  filterLi f str =
    li' @ class_ "filter" . selectedFilter f $ do
      a
        @ onClick (Filter f)
        . att "href" "" -- harmless empty href is for the CSS
        $ text str
  selectedFilter f =
    if f == filt then class_ "selected" else id

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
  li'
    @ onDblClick (Edit filt todo)
    . bool id (class_ "completed") todo.completed
    . style' "border-bottom: 1px solid #ededed"
    $ do
      div' @ class_ "view" $ do
        target AllTodos $ do
          input'
            @ class_ "toggle"
            . att "type" "checkbox"
            . onClick (SetCompleted filt todo $ not todo.completed)
            . checked todo.completed

          label' @ class_ "label" $ do
            text todo.task

          -- FIXME: create a Destroy action variant (I think it's missing from the original impl)
          button (SetCompleted filt todo $ not todo.completed) @ class_ "destroy" $ ""

todoEditView :: FilterTodo -> Todo -> View TodoView ()
todoEditView filt todo = do
  let f = fieldNames @TodoForm
  li' @ class_ "editing" $ do
    form (SubmitEdit filt todo) $ do
      let taskField = Input f.task
      -- Instead of using the `field` FormField wrapper, we add the context manually
      -- and use a custom input field for maximum control over the generated HTML
      let Input (FieldName fn) = taskField
      addContext taskField $ do
        input'
          @ class_ "edit"
          . value todo.task
          . autofocus
          . name fn -- because we use a custom input, we must provide this param for the library

--- Helpers ----------------------------------------------------------------------------

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

style' :: (Attributable h) => AttValue -> Attributes h -> Attributes h
style' = att "style"
