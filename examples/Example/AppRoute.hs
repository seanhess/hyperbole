{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Example.AppRoute where

import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Text.Casing (fromHumps, toWords)
import Text.Read (readMaybe)
import Web.Hyperbole
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Route

type UserId = Int

data AppRoute
  = Main
  | Intro
  | Basics
  | CSS
  | Simple
  | Hello Hello
  | Contacts ContactRoute
  | Interactivity
  | State StateRoute
  | Counter
  | Forms FormRoute
  | Requests
  | Response
  | Concurrency
  | Data DataRoute
  | Examples ExamplesRoute
  | Errors
  | Javascript
  | Advanced
  | OAuth2Authenticate
  | OAuth2
  | Test
  | Chat
  deriving (Eq, Generic, Show)
instance Route AppRoute where
  baseRoute = Just Main

-- -- View Route
-- data IntroRoute
--   = IntroMain
--   | Pages
--   | Views
--   | HyperViews
--   | ViewFunctions
--   | CSS CSSRoute
--   deriving (Eq, Generic, Show)
-- instance Route IntroRoute where
--   baseRoute = Just IntroMain

data Basics
  = Pages
  | HtmlViews
  | Interactive
  deriving (Show, Enum, Bounded)
instance PageAnchor Basics where
  sectionTitle Interactive = "Interactive HyperViews"
  sectionTitle HtmlViews = "HTML Views"
  sectionTitle b = cs (show b)

  navEntry Interactive = "HyperViews"
  navEntry a = sectionTitle a

data FormRoute
  = FormSimple
  | FormValidation
  deriving (Eq, Generic, Show)
instance Route FormRoute where
  baseRoute = Just FormSimple

data CSSExample
  = Transitions
  | Tooltips
  | External
  deriving (Eq, Generic, Show, Enum, Bounded)
instance PageAnchor CSSExample where
  sectionTitle = \case
    Transitions -> "CSS Transitions"
    Tooltips -> "Tooltips"
    External -> "External Stylesheets"
instance ExampleSource CSSExample where
  exampleSource c = ["Example", "Page", "Intro", "CSS", cs (show c) <> ".hs"]

data DataRoute
  = DataLists
  | SortableTable
  | Autocomplete
  | Filter
  | LoadMore
  deriving (Eq, Generic, Show)
instance Route DataRoute where
  baseRoute = Just DataLists

data StateRoute
  = StateRoot
  | Actions
  | StateView
  | Effects
  | Query
  | Sessions
  deriving (Eq, Generic, Show)
instance Route StateRoute where
  baseRoute = Just StateRoot

data ContactRoute
  = ContactsAll
  | Contact UserId
  deriving (Eq, Generic, Show)
instance Route ContactRoute where
  baseRoute = Just ContactsAll

  matchRoute [contactId] = do
    cid <- readMaybe $ unpack contactId
    pure $ Contact cid
  matchRoute [] = pure ContactsAll
  matchRoute other = genMatchRoute other.segments

  routePath (Contact uid) = routePath uid
  routePath ContactsAll = []

data ExamplesRoute
  = BigExamples
  | Todos
  | TodosCSS -- A version using the CSS from TodoMVC project
  deriving (Eq, Generic, Show)
instance Route ExamplesRoute where
  baseRoute = Just BigExamples

data Hello
  = Greet Text
  | Redirected
  | RedirectNow
  deriving (Eq, Generic, Route, Show)

routeTitle :: AppRoute -> Text
routeTitle (Hello _) = "Hello World"
-- routeTitle (Intro IntroMain) = "Intro"
-- routeTitle (Intro (CSS _)) = "Atomic CSS"
-- routeTitle (Intro r) = defaultTitle r
routeTitle (Contacts ContactsAll) = "Contacts"
routeTitle (State Effects) = "Effects"
routeTitle (State StateRoot) = "State"
routeTitle (State StateView) = "Built-in State"
routeTitle (State Actions) = "Managing State"
routeTitle (State Query) = "Query"
routeTitle (State Sessions) = "Sessions"
routeTitle (Forms FormSimple) = "Forms"
routeTitle (Forms FormValidation) = "Form Validation"
routeTitle (Data d) = defaultTitle d
routeTitle Errors = "Error Handling"
routeTitle (Examples Todos) = "TodoMVC"
routeTitle (Examples TodosCSS) = "TodoMVC (CSS version)"
routeTitle (Examples BigExamples) = "Large Examples"
routeTitle OAuth2 = "OAuth2"
routeTitle r = defaultTitle r

instance ExampleSource AppRoute where
  exampleSource (State s) = ["Example", "Page", "State", cs (show s) <> ".hs"]
  exampleSource (Contacts (Contact _)) = "Example/Page/Contact.hs"
  exampleSource (Contacts ContactsAll) = "Example/Page/Contacts.hs"
  -- routeSource (Intro (CSS CSSAll)) = ["Example", "Page", "Intro", "CSS.hs"]
  -- routeSource (Intro (CSS c)) = ["Example", "Page", "Intro", "CSS", cs (show c) <> ".hs"]
  exampleSource (Data SortableTable) = "Example/Page/DataLists/DataTable.hs"
  exampleSource (Data d) = ["Example", "Page", "DataLists", cs (show d) <> ".hs"]
  exampleSource (Forms f) = ["Example", "Page", "Forms", cs (show f) <> ".hs"]
  exampleSource r = ["Example", "Page", cs (show r) <> ".hs"]

defaultTitle :: (Show r) => r -> Text
defaultTitle = cs . toWords . fromHumps . show

class ExampleSource a where
  exampleSource :: a -> Path

class PageAnchor n where
  pageAnchor :: n -> Text
  default pageAnchor :: n -> Text
  pageAnchor = T.toLower . T.replace " " "-" . sectionTitle

  sectionTitle :: n -> Text
  default sectionTitle :: (Show n) => n -> Text
  sectionTitle = cs . toWords . fromHumps . show

  navEntry :: n -> Text
  default navEntry :: n -> Text
  navEntry = sectionTitle
