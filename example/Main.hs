{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.State.Static.Local
import Example.AppRoute
import Example.Colors
import Example.Concurrent qualified as Concurrent
import Example.Contact qualified as Contact
import Example.Contacts qualified as Contacts
import Example.Counter qualified as Counter
import Example.Effects.Debug as Debug
import Example.Effects.Users as Users
import Example.Errors qualified as Errors
import Example.Forms qualified as Forms
import Example.LazyLoading qualified as LazyLoading
import Example.Redirects qualified as Redirects
import Example.Requests qualified as Requests
import Example.Search qualified as Search
import Example.Sessions qualified as Sessions
import Example.Simple qualified as Simple
import Example.Style qualified as Style
import Example.Transitions qualified as Transitions
import Example.View.Layout (exampleLayout)
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Web.Hyperbole
import Web.Hyperbole.Effect.Handler (RunHandlers)
import Web.Hyperbole.Effect.Server (Request (..))


-- import Network.Wai.Handler.WebSockets (websocketsOr)

main :: IO ()
main = do
  putStrLn "Starting Examples on http://localhost:3000"
  users <- Users.initUsers
  count <- runEff $ runConcurrent Counter.initCounter
  Warp.run 3000 $
    staticPolicy (addBase "client/dist") $
      staticPolicy (addBase "static") $
        app users count


app :: UserStore -> TVar Int -> Application
app users count = do
  liveApp
    toDocument
    (runApp . routeRequest $ router)
 where
  runApp :: (IOE :> es) => Eff (Concurrent : Debug : Users : es) a -> Eff es a
  runApp = runUsersIO users . runDebugIO . runConcurrent

  router :: forall es. (Hyperbole :> es, Users :> es, Debug :> es, Concurrent :> es, IOE :> es) => AppRoute -> Eff es Response
  router (Hello h) = runPage $ hello h
  router Simple = runPage Simple.simplePage
  router (Contacts ContactsAll) = runPage Contacts.page
  router (Contacts (Contact uid)) = Contact.response uid
  router Counter = runReader count $ runPage Counter.page
  router Transitions = runPage Transitions.page
  router Forms = runPage Forms.page
  router Sessions = runPage Sessions.page
  router LazyLoading = runPage LazyLoading.page
  router Concurrent = runPage Concurrent.page
  router Requests = runPage Requests.page
  router Redirects = runPage Redirects.page
  router LiveSearch = runPage Search.page
  router Errors = runPage Errors.page
  router RedirectNow = do
    redirect (routeUrl $ Hello Redirected)
  router Query = do
    p <- reqParam "key"
    view $ el (pad 20) $ do
      text "key: "
      text p
  router Main = do
    redirect (routeUrl Simple)

  -- Nested Router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es '[]
  hello (Greet who) = do
    pure $ exampleLayout (Hello $ Greet who) $ do
      row (gap 6 . pad 10) $ do
        el_ "Hello:"
        el_ $ text who
  hello Redirected = do
    pure $ exampleLayout RedirectNow $ el (pad 10) "You were redirected"

  -- Use the embedded version for real applications (see basicDocument).
  -- The link to /hyperbole.js here is just to make local development easier
  toDocument :: BL.ByteString -> BL.ByteString
  toDocument cnt =
    [i|<html>
      <head>
        <title>Hyperbole Examples</title>
        <script type="text/javascript" src="/hyperbole.js"></script>
        <style type="text/css">#{cssResetEmbed}</style>
        <style type="text/css">body { background-color: \#d3dceb }</style>
      </head>
      <body>#{cnt}</body>
    </html>|]
