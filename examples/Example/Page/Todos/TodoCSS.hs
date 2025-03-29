{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Todos.TodoCSS (page) where

import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Text qualified as T

import Example.Effects.Todos (Todo, TodoId, Todos)
import Example.Effects.Todos qualified as Todos
import Example.Page.Todos.Shared (FilterTodo (..), TodoForm (..), pluralize)
import Web.Hyperbole as Hyperbole
import Web.Hyperbole.View.Forms (Input (Input))
import Web.View.Style (extClass)
import Web.View.Types (AttValue)

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
    div' id $ do
      stylesheet "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"
      section (extClass "todoapp") $ do
        hyper AllTodos $ todosView FilterAll todos
      footer (extClass "info") $ do
        let p_ = p (style' "margin: 1em auto")
        p_ "Double-click to edit a todo"
        p_ $ do
          span' id "Go back to the "
          a
            ( att "href" "/"
                . style' "color: #b83f45"
            )
            "index"

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
  header (extClass "header") $ do
    h1 (style' "top:-80px") $ text "todos"
    todoForm
  main' (extClass "main") $ do
    div' (extClass "toggle-all-container") $ do
      input'
        ( extClass "toggle-all"
            . att "id" "toggle-all"
            . att "type" "checkbox"
        )
      label'
        ( extClass "toggle-all-label"
            . att "for" "toggle-all"
            . onClick (ToggleAll filt)
        )
        (text "Mark all as complete")
    ul' (extClass "todo-list") $ do
      forM_ todos $ \todo -> do
        hyper (TodoView todo.id) $ todoView filt todo
  statusBar filt todos

todoForm :: View AllTodos ()
todoForm = do
  let f :: TodoForm FieldName = fieldNames
  form SubmitTodo grow $ do
    field f.task id $ do
      Input (FieldName nm) <- context
      input' -- we use a custom input field, because the Hyperbole one overrides autocomplete
        ( extClass "new-todo"
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
        )

statusBar :: FilterTodo -> [Todo] -> View AllTodos ()
statusBar filt todos = do
  footer (extClass "footer" . style' "padding-bottom: 30px") $ do
    let numLeft = length $ filter (\t -> not t.completed) todos
    span' (extClass "todo-count") $ do
      text $
        mconcat
          [ T.pack $ show numLeft
          , " "
          , pluralize numLeft "item" "items"
          , " "
          , "left!"
          ]
    space
    ul' (extClass "filters") $ do
      filterLi FilterAll "All"
      filterLi Active "Active"
      filterLi Completed "Completed"
    space
    button ClearCompleted (extClass "clear-completed") "Clear completed"
 where
  filterLi f str =
    li' (extClass "filter" . selectedFilter f) $ do
      a
        ( onClick (Filter f)
            . att "href" "" -- harmless empty href is for the CSS
        )
        (text str)
  selectedFilter f =
    if f == filt then extClass "selected" else id

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
    ( onDblClick (Edit filt todo)
        . bool id (extClass "completed") todo.completed
        . style' "border-bottom: 1px solid #ededed"
    )
    $ do
      div' (extClass "view") $ do
        target AllTodos $ do
          input'
            ( extClass "toggle"
                . att "type" "checkbox"
                . onClick (SetCompleted filt todo $ not todo.completed)
                . checked todo.completed
            )
        label' (extClass "label") $ do
          text todo.task

todoEditView :: FilterTodo -> Todo -> View TodoView ()
todoEditView filt todo = do
  let f = fieldNames @TodoForm
  li' (extClass "editing") $ do
    form (SubmitEdit filt todo) id $ do
      let taskField = (Input f.task)
      -- Instead of using the `field` FormField wrapper, we add the context manually
      -- and use a custom input field for maximum control over the generated HTML
      let Input (FieldName fn) = taskField
      addContext taskField $ do
        input'
          ( extClass "edit"
              . value todo.task
              . autofocus
              . extClass "hello"
              . name fn -- because we use a custom input, we must provide this param for the library
          )

--- Helpers ----------------------------------------------------------------------------

div' :: Mod c -> View c () -> View c ()
div' = tag "div"

span' :: Mod c -> View c () -> View c ()
span' = tag "span"

section :: Mod c -> View c () -> View c ()
section = tag "section"

header :: Mod c -> View c () -> View c ()
header = tag "header"

main' :: Mod c -> View c () -> View c ()
main' = tag "main"

h1 :: Mod c -> View c () -> View c ()
h1 = tag "h1"

p :: Mod c -> View c () -> View c ()
p = tag "p"

label' :: Mod c -> View c () -> View c ()
label' = tag "label"

input' :: Mod c -> View c ()
input' m = tag "input" m ""

a :: Mod c -> View c () -> View c ()
a = tag "a"

ul' :: Mod c -> View c () -> View c ()
ul' = tag "ul"

li' :: Mod c -> View c () -> View c ()
li' = tag "li"

footer :: Mod c -> View c () -> View c ()
footer = tag "footer"

style' :: AttValue -> Mod c
style' = att "style"