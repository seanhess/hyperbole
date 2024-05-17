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
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Example.Contacts qualified as Contacts
import Example.Counter qualified as Counter
import Example.Effects.Debug as Debug
import Example.Effects.Users as Users
import Example.Errors qualified as Errors
import Example.Forms qualified as Forms
import Example.LazyLoading qualified as LazyLoading
import Example.Redirects qualified as Redirects
import Example.Sessions qualified as Sessions
import Example.Transitions qualified as Transitions
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Web.Hyperbole
import Web.Hyperbole.Effect (Request (..))


-- import Network.Wai.Handler.WebSockets (websocketsOr)

main :: IO ()
main = do
  putStrLn "Starting Examples on http://localhost:3003"
  users <- initUsers
  count <- runEff $ runConcurrent $ newTVarIO 0
  Warp.run 3003 $
    staticPolicy (addBase "client/dist") $
      app users count


data AppRoute
  = Main
  | Hello Hello
  | Contacts
  | Transitions
  | Query
  | Counter
  | Forms
  | Sessions
  | Redirects
  | RedirectNow
  | LazyLoading
  | Errors
  deriving (Eq, Generic, Route)


data Hello
  = Greet Text
  | Redirected
  deriving (Eq, Generic, Route)


app :: UserStore -> TVar Int -> Application
app users count = do
  liveApp
    toDocument
    (runApp . routeRequest $ router)
 where
  runApp :: (IOE :> es) => Eff (Concurrent : Debug : Users : es) a -> Eff es a
  runApp = runUsersIO users . runDebugIO . runConcurrent

  router :: (Hyperbole :> es, Users :> es, Debug :> es, Concurrent :> es, IOE :> es) => AppRoute -> Eff es Response
  router (Hello h) = page $ hello h
  router Contacts = page Contacts.page
  router Counter = page $ Counter.page count
  router Transitions = page Transitions.page
  router Forms = page Forms.page
  router Sessions = page Sessions.page
  router LazyLoading = page LazyLoading.page
  router Redirects = page Redirects.page
  router Errors = page Errors.page
  router RedirectNow = do
    redirect (routeUrl $ Hello Redirected)
  router Query = do
    p <- reqParam "key"
    view $ el (pad 20) $ do
      text "key: "
      text p
  router Main = do
    -- send $ SetSession @Text "woot" "hello"
    view $ do
      col (gap 10 . pad 20) $ do
        el (bold . fontSize 32) "Examples"
        route (Hello (Greet "World")) id "Hello World"
        route Counter id "Counter"
        route Contacts id "Contacts"
        route Transitions id "Transitions"
        route Forms id "Forms"
        link "/query?key=value" id "Query Params"
        route Sessions id "Sessions"
        route Redirects id "Redirects"
        route RedirectNow id "Redirect Now"
        route LazyLoading id "Lazy Loading"
        route Errors id "Errors"

  -- example sub-router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es Response
  hello Redirected = load $ do
    pure $ el_ "You were redirected"
  hello (Greet s) = load $ do
    r <- request
    pure $ col (gap 10 . pad 10) $ do
      el_ $ do
        text "Greetings: "
        text s
      el_ $ do
        text "Host: "
        text $ cs $ show r.host
      el_ $ do
        text "Path: "
        text $ cs $ show r.path
      el_ $ do
        text "Query: "
        text $ cs $ show r.query
      el_ $ do
        text "Cookies: "
        text $ cs $ show r.cookies

  -- Use the embedded version for real applications (see below). The link to /hyperbole.js here is just to make local development easier
  toDocument :: BL.ByteString -> BL.ByteString
  toDocument cnt =
    [i|<html>
      <head>
        <title>Hyperbole Examples</title>
        <script type="text/javascript" src="/hyperbole.js"></script>
        <style type type="text/css">#{cssResetEmbed}</style>
      </head>
      <body>#{cnt}</body>
    </html>|]

-- toDocument :: BL.ByteString -> BL.ByteString
-- toDocument cnt =
--   [i|<html>
--     <head>
--       <title>Hyperbole Examples</title>
--       <script type="text/javascript">#{scriptEmbed}</script>
--       <style type type="text/css">#{cssResetEmbed}</style>
--     </head>
--     <body>#{cnt}</body>
--   </html>|]
