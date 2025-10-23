{-# LANGUAGE OverloadedLists #-}

module Example.AppRoute where

import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import Text.Casing (fromHumps, toWords)
import Text.Read (readMaybe)
import Web.Hyperbole
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Route

type UserId = Int

data AppRoute
  = Main
  | Intro
  | Simple
  | Hello Hello
  | Contacts ContactRoute
  | CSS CSSRoute
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

data FormRoute
  = FormSimple
  | FormValidation
  deriving (Eq, Generic, Show)
instance Route FormRoute where
  baseRoute = Just FormSimple

data CSSRoute
  = CSSAll
  | Transitions
  | Tooltips
  | External
  deriving (Eq, Generic, Show)
instance Route CSSRoute where
  baseRoute = Just CSSAll

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
routeTitle (Contacts ContactsAll) = "Contacts (Advanced)"
routeTitle (State Effects) = "Effects"
routeTitle (State StateRoot) = "State"
routeTitle (State Actions) = "Action Context"
routeTitle (State Query) = "Query"
routeTitle (State Sessions) = "Sessions"
routeTitle (Forms FormSimple) = "Forms"
routeTitle (Forms FormValidation) = "Form Validation"
routeTitle (Data d) = defaultTitle d
routeTitle Errors = "Error Handling"
routeTitle (Examples Todos) = "TodoMVC"
routeTitle (Examples TodosCSS) = "TodoMVC (CSS version)"
routeTitle (Examples BigExamples) = "Large Examples"
routeTitle (CSS _) = "CSS"
routeTitle OAuth2 = "OAuth2"
routeTitle r = defaultTitle r

routeSource :: AppRoute -> Path
routeSource (State s) = ["Example", "Page", "State", cs (show s) <> ".hs"]
routeSource (Contacts (Contact _)) = "Example/Page/Contact.hs"
routeSource (Contacts ContactsAll) = "Example/Page/Contacts.hs"
routeSource (CSS CSSAll) = ["Example", "Page", "CSS.hs"]
routeSource (CSS c) = ["Example", "Page", "CSS", cs (show c) <> ".hs"]
routeSource (Data SortableTable) = "Example/Page/DataLists/DataTable.hs"
routeSource (Data d) = ["Example", "Page", "DataLists", cs (show d) <> ".hs"]
routeSource (Forms f) = ["Example", "Page", "Forms", cs (show f) <> ".hs"]
routeSource r = ["Example", "Page", cs (show r) <> ".hs"]

defaultTitle :: (Show r) => r -> Text
defaultTitle = cs . toWords . fromHumps . show
