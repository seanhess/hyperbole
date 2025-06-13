{-# LANGUAGE LambdaCase #-}

module Example.View.Layout where

import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Version (showVersion)
import Example.AppRoute
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.View.Icon as Icon (hamburger)
import Paths_examples (version)
import Text.Casing (fromHumps, toWords)
import Web.Hyperbole
import Web.View.Style (addClass, cls, prop)
import Web.View.Types (ChildCombinator (..), Class (..), Selector (..), selector)

exampleLayout :: AppRoute -> View c () -> View c ()
exampleLayout rt pageView = do
  rootLayout rt $ do
    pageDescription rt
    link sourceUrl Style.link "View Source"
    row (bg White) $ do
      pageView
 where
  sourceUrl = "https://github.com/seanhess/hyperbole/blob/0.4/example/" <> routeSource rt

  routeSource :: AppRoute -> Url
  routeSource = \case
    Simple -> "Example/Page/Simple.hs"
    Contacts ContactsAll -> "Example/Page/Contacts.hs"
    Contacts (Contact _) -> "Example/Page/Contact.hs"
    Counter -> "Example/Page/Counter.hs"
    Transitions -> "Example/Page/Transitions.hs"
    FormSimple -> "Example/Page/FormSimple.hs"
    FormValidation -> "Example/Page/FormValidation.hs"
    Sessions -> "Example/Page/Sessions.hs"
    LazyLoading -> "Example/Page/LazyLoading.hs"
    Concurrent -> "Example/Page/Concurrent.hs"
    Redirects -> "Example/Page/Redirects.hs"
    Requests -> "Example/Page/Requests.hs"
    Filter -> "Example/Page/Filter.hs"
    Autocomplete -> "Example/Page/Autocomplete.hs"
    Errors -> "Example/Page/Errors.hs"
    RedirectNow -> "Main.hs"
    Query -> "Main.hs"
    Hello _ -> "Main.hs"
    Main -> "Main.hs"
    Examples -> "Example/View/Layout.hs"
    Todos -> "Example/Page/Todo.hs"
    DataTable -> "Example/Page/DataTable.hs"
    Javascript -> "Example/Page/Javascript.hs"

rootLayout :: AppRoute -> View c () -> View c ()
rootLayout rt content =
  layout id $ do
    el (grow . flexRow . onMobile flexCol) $ do
      navigation rt
      col (pad 20 . gap 20 . grow) $ do
        content

exampleMenu :: AppRoute -> View c ()
exampleMenu current = do
  example Simple
  example Counter
  example Transitions
  example Requests
  example Redirects
  example RedirectNow
  example LazyLoading
  example Concurrent
  example FormSimple
  example FormValidation
  example DataTable
  example Sessions
  example Filter
  example Autocomplete
  example Todos
  example (Contacts ContactsAll)
  example Javascript
 where
  -- example Errors

  -- link "/query?key=value" lnk "Query Params"

  example rt =
    route rt (menuItem . selected rt) (text $ routeTitle rt)
  menuItem =
    pad (XY 20 10) . color White . hover (bg DarkHighlight)
  selected rt =
    if rt == current then bg DarkHighlight else id

routeTitle :: AppRoute -> Text
routeTitle (Hello _) = "Hello World"
routeTitle (Contacts ContactsAll) = "Contacts (Advanced)"
routeTitle Filter = "Search - Filters"
routeTitle Autocomplete = "Search - Autocomplete"
routeTitle Todos = "TodoMVC"
routeTitle FormSimple = "Forms - Simple"
routeTitle FormValidation = "Forms - Validation"
routeTitle r = cs $ toWords $ fromHumps $ show r

pageDescription :: AppRoute -> View c ()
pageDescription = \case
  Simple -> do
    el_ "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
    el_ "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"
  Counter -> do
    el_ $ do
      text "Pages and update functions can run side effects before rendering. Here we use a "
      code id "Reader (TVar Int)"
      text "to track the count"
    ol (pad (XY 15 0)) $ do
      item $ do
        text "Uses a view function to render the state: "
        code id "viewCount :: Int -> View Counter ()"
    el_ "Notice how the view function expects the current count as a parameter"
  Transitions -> do
    el_ "We can use web-view to animate transitions"
  Sessions -> do
    el_ "Reload your browser after changing these settings to see the session information preserved"
  Requests -> do
    el_ "The Hyperbole Effect allows us to access the Request, and manipulate the Client"
  Redirects -> none
  RedirectNow -> none
  LazyLoading -> do
    el_ "We can use onLoad to lazily load content and poll for changes"
  FormSimple -> do
    el_ "Use a Higher Kinded Type to define form fields"
  FormValidation ->
    el_ $ do
      code id "instance Form MyForm Validated"
      text " allows us to manage validation states for each field"
  Filter ->
    el_ "Easily serialize a datatype to the querystring, preserving faceted search in the url"
  Autocomplete ->
    el_ "Create a serverside autocomplete with a combination of onInput and onKeyDown"
  DataTable -> do
    el_ "Complex reusable View Functions allow us to reuse code via composition instead of inheritance"
  Concurrent ->
    el_ "Separate HyperViews can overlap updates without issues"
  Todos ->
    row (gap 5) $ do
      el_ "Implementation of "
      link "https://todomvc.com/" Style.link "TodoMVC"
  Contacts _ -> do
    el_ "This complex example combines various features"
  Examples -> none
  Errors -> none
  Main -> none
  Hello _ -> none
  Query -> none
  Javascript -> none
 where
  item = li (list Disc)

examplesView :: View c ()
examplesView = rootLayout Examples $ do
  col (bg Dark) $ do
    exampleMenu Examples

navigation :: AppRoute -> View c ()
navigation rt = do
  nav (bg Dark . color White . flexCol . hover showMenu) $ do
    row id $ do
      link "https://github.com/seanhess/hyperbole" (bold . pad 20 . logo . width 220) "HYPERBOLE"
      space
      el (hide . onMobile flexCol) $ do
        el (pad 6) $ do
          el (color White . width 50 . height 50) Icon.hamburger
    col (onMobile hide . menuTarget) $ do
      exampleMenu rt
      space
      el (pad 10 . fontSize 12) $ do
        text "v"
        text $ cs $ showVersion version
 where
  showMenu =
    addClass $
      Class menuSelector mempty
        & prop @Text "display" "flex"

  menuTarget = addClass $ cls "menu"

  menuSelector = (selector "menu-parent"){child = Just (ChildWithName "menu")}

  -- https://www.fontspace.com/super-brigade-font-f96444
  logo =
    addClass $
      cls "logo"
        & prop @Text "background" "no-repeat center/90% url(/logo-robot.png)"
        & prop @Text "color" "transparent"

onMobile :: Mod c -> Mod c
onMobile = media (MaxWidth 650)
