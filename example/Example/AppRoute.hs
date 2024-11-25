module Example.AppRoute where

import Data.Text (Text)
import Example.Effects.Users (UserId)
import Web.Hyperbole


data AppRoute
  = Main
  | Concurrent
  | Contacts ContactRoute
  | Counter
  | Errors
  | Forms
  | Hello Hello
  | LazyLoading
  | LiveSearch
  | Query
  | RedirectNow
  | Redirects
  | Sessions
  | Simple
  | Transitions
  | Triggers
  deriving (Eq, Generic)
instance Route AppRoute where
  baseRoute = Just Main


data ContactRoute
  = ContactsAll
  | Contact UserId
  deriving (Eq, Generic)
instance Route ContactRoute where
  baseRoute = Just ContactsAll


data Hello
  = Greet Text
  | Redirected
  deriving (Eq, Generic, Route)
