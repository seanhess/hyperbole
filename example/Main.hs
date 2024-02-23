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
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Example.Contacts qualified as Contacts
import Example.Effects.Debug as Debug
import Example.Effects.Users as Users
import Example.Forms qualified as Forms
import Example.Transitions qualified as Transitions
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Web.Hyperbole


-- import Network.Wai.Handler.WebSockets (websocketsOr)

main :: IO ()
main = do
  putStrLn "Starting Examples on http://localhost:3003"
  users <- initUsers
  Warp.run 3003 $
    staticPolicy (addBase "client/dist") $
      app users


data AppRoute
  = Main
  | Hello Hello
  | Contacts
  | Transitions
  | Query
  | Forms
  deriving (Show, Generic, Eq, Route)


data Hello
  = Greet Text
  deriving (Show, Generic, Eq, Route)


app :: UserStore -> Application
app users = do
  liveApp
    toDocument
    (runApp . routeRequest $ router)
    (runApp . routeRequest $ router)
 where
  runApp :: (IOE :> es) => Eff (Debug : Users : es) a -> Eff es a
  runApp = runUsersIO users . runDebugIO

  router :: (Hyperbole :> es, Users :> es, Debug :> es) => AppRoute -> Eff es Response
  router (Hello h) = page $ hello h
  router Contacts = page Contacts.page
  router Transitions = page Transitions.page
  router Forms = page Forms.page
  router Query = do
    p <- reqParam "key"
    view $ do
      text "test: "
      text p
  router Main = do
    ps <- reqParams
    Debug.dump "Query Params" ps
    view $ do
      col (gap 10 . pad 10) $ do
        el (bold . fontSize 32) "Examples"
        link (Hello (Greet "World")) id "Hello World"
        link Contacts id "Contacts"
        link Transitions id "Transitions"
        link Forms id "Forms"
        tag "a" (att "href" "/query?key=value") "Query Params"

  -- example sub-router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es Response
  hello (Greet s) = load $ do
    pure $ do
      el (pad 10 . gap 10) $ do
        text "Greetings, "
        text s

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
