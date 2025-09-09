{-# LANGUAGE OverloadedLists #-}


module Example.AppRoute where

import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import Text.Casing (fromHumps, toWords)
import Text.Read (readMaybe)
import Web.Hyperbole

type UserId = Int

data AppRoute
  = Main
  | Intro
  | Simple
  | Hello Hello
  | Contacts ContactRoute
  | CSS
  | Interactivity
  | State StateRoute
  | Counter
  | Forms
  | Requests
  | Concurrency
  | Data DataRoute
  | Examples ExamplesRoute
  | Errors
  | Javascript
  | Advanced
  | OAuth2Authenticate
  | OAuth2
  | Test
  deriving (Eq, Generic, Show)
instance Route AppRoute where
  baseRoute = Just Main

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

  matchRoute [] = pure ContactsAll
  matchRoute [""] = pure ContactsAll
  matchRoute [contactId] = do
    cid <- readMaybe $ unpack contactId
    pure $ Contact cid
  matchRoute _ = Nothing

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
routeTitle (Data d) = defaultTitle d
routeTitle Errors = "Error Handling"
routeTitle (Examples Todos) = "TodoMVC"
routeTitle (Examples TodosCSS) = "TodoMVC (CSS version)"
routeTitle (Examples BigExamples) = "Large Examples"
routeTitle OAuth2 = "OAuth2"
routeTitle r = defaultTitle r

defaultTitle :: (Show r) => r -> Text
defaultTitle = cs . toWords . fromHumps . show
