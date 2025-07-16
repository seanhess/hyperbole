module Example.AppRoute where

import Data.Text (Text, unpack)
import Example.Effects.Users (UserId)
import Text.Read (readMaybe)
import Web.Hyperbole

data AppRoute
  = Main
  | Intro
  | Simple
  | Hello Hello
  | Contacts ContactRoute
  | AtomicCSS
  | State StateRoute
  | Counter
  | Forms
  | Requests
  | Concurrency
  | Data DataRoute
  | Todos
  | Errors
  | Javascript
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

data Hello
  = Greet Text
  | Redirected
  deriving (Eq, Generic, Route, Show)
