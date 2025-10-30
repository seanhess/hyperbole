{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Docs.MultiPage where

import Example.Docs.Interactive qualified as Message
import Example.Docs.MultiView qualified as Counter
import Web.Atomic.CSS
import Web.Hyperbole

data AppRoute
  = Message -- /message
  | Counter -- /counter
  deriving (Generic, Eq, Route)

main = do
  app <- liveApp quickStartDocument (routeRequest router)
  run 3000 app
 where
  router Message = runPage Message.page
  router Counter = runPage Counter.page

menu :: View c ()
menu = do
  route Message "Link to /message"
  route Counter "Link to /counter"

exampleLayout :: View c () -> View c ()
exampleLayout contents = do
  col ~ grow $ do
    el ~ border 1 $ "My Website Header"
    row $ do
      menu
      contents

examplePage :: Page es '[]
examplePage = do
  pure $ exampleLayout $ do
    el "page contents"
