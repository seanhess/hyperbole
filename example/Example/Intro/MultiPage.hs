{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Intro.MultiPage where

import Example.Intro.Interactive qualified as Message
import Example.Intro.MultiView qualified as Counter
import Web.Hyperbole

data AppRoute
  = Message -- /message
  | Counter -- /counter
  deriving (Generic, Eq, Route)

main = do
  run 3000 $ do
    liveApp (basicDocument "Multiple Pages") (routeRequest router)
 where
  router Message = runPage Message.page
  router Counter = runPage Counter.page

menu :: View c ()
menu = do
  route Message id "Link to /message"
  route Counter id "Link to /counter"

exampleLayout :: View c () -> View c ()
exampleLayout content = do
  layout id $ do
    el (border 1) "My Website Header"
    row id $ do
      menu
      content

examplePage :: Eff es (Page '[])
examplePage = do
  pure $ exampleLayout $ do
    el_ "page contents"
