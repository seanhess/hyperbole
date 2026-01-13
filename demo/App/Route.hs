{-# LANGUAGE OverloadedLists #-}

module App.Route where

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
  | Basics
  | CSS
  | Simple
  | Hello Hello
  | Contacts ContactRoute
  | Interactivity
  | SideEffects
  | Hyperviews
  | State
  | Counter
  | Forms FormRoute
  | HyperboleEffect
  | Response
  | Concurrency
  | Data DataRoute
  | Examples ExamplesRoute
  | Errors
  | Javascript
  | Test
  | Chat
  | ViewFunctions
  | Application
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

data FormRoute
  = FormSimple
  | FormValidation
  deriving (Eq, Generic, Show)
instance Route FormRoute where
  baseRoute = Just FormSimple

data DataRoute
  = DataLists
  | SortableTable
  | Autocomplete
  | Filter
  | LoadMore
  deriving (Eq, Generic, Show)
instance Route DataRoute where
  baseRoute = Just DataLists

-- data StateRoute
--   = StateRoot
--   | Actions
--   | StateView
--   | Effects
--   | Query
--   | Sessions
--   deriving (Eq, Generic, Show)
-- instance Route StateRoute where
--   baseRoute = Just StateRoot

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
  = OtherExamples
  | Todos
  | TodosCSS -- A version using the CSS from TodoMVC project
  | Tags
  | OAuth2Authenticate
  | OAuth2
  deriving (Eq, Generic, Show)
instance Route ExamplesRoute where
  baseRoute = Just OtherExamples

data Hello
  = Greet Text
  | Redirected
  | RedirectNow
  deriving (Eq, Generic, Route, Show)

routeTitle :: AppRoute -> Text
routeTitle (Hello _) = "Hello World"
routeTitle CSS = "Styles"
-- routeTitle (Intro IntroMain) = "Intro"
-- routeTitle (Intro (CSS _)) = "Atomic CSS"
-- routeTitle (Intro r) = defaultTitle r
routeTitle (Contacts ContactsAll) = "Contacts"
routeTitle State = "Managing State"
routeTitle Hyperviews = "More HyperViews"
-- routeTitle (State StateRoot) = "State"
-- routeTitle (State StateView) = "Built-in State"
-- routeTitle (State Actions) = "Managing State"
-- routeTitle (State Query) = "Query"
-- routeTitle (State Sessions) = "Sessions"
routeTitle (Forms FormSimple) = "Forms"
routeTitle (Forms FormValidation) = "Form Validation"
routeTitle (Data d) = defaultTitle d
routeTitle Errors = "Error Handling"
routeTitle (Examples Todos) = "TodoMVC"
routeTitle (Examples TodosCSS) = "TodoMVC (CSS version)"
routeTitle (Examples OAuth2) = "OAuth2"
routeTitle (Examples OtherExamples) = "Examples"
routeTitle (Examples e) = defaultTitle e
routeTitle r = defaultTitle r

defaultTitle :: (Show r) => r -> Text
defaultTitle = cs . toWords . fromHumps . show
