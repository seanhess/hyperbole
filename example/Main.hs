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
import Effectful.State.Static.Local
import Example.Contacts qualified as Contacts
import Example.Effects.Debug
import Example.Effects.Users as Users
import Example.Layout qualified as Layout
import Example.Transitions qualified as Transitions
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai ()
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Hyperbole
import Web.View

main :: IO ()
main = do
  putStrLn "Starting Examples on http://localhost:3003"
  users <- initUsers
  Warp.run 3003
    $ staticPolicy (addBase "client/dist")
    $ app users

data AppRoute
  = Main
  | Hello Hello
  | Contacts
  | Layout
  | Transitions
  | Echo
  deriving (Show, Generic, Eq, Route)

data Hello
  = Greet Text
  deriving (Show, Generic, Eq, Route)

app :: UserStore -> Application
app users = waiApplication document (runUsersIO users . runHyperbole . runDebugIO . router)
 where
  router :: (Hyperbole :> es, Users :> es, Debug :> es) => AppRoute -> Eff es ()
  router (Hello h) = page $ hello h
  router Echo = page $ load $ do
    f <- formData
    pure $ col id $ do
      el id "ECHO:"
      text $ cs $ show f
  router Contacts = page Contacts.page
  router Layout = page Layout.page
  router Transitions = page Transitions.page
  router Main = view $ do
    col (gap 10 . pad 10) $ do
      el (bold . fontSize 32) "Examples"
      link (Hello (Greet "World")) id "Hello World"
      link Contacts id "Contacts"
      link Layout id "Layout"
      link Transitions id "Transitions"

  -- example sub-router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es ()
  hello (Greet s) = load $ do
    pure $ do
      el (pad 10 . gap 10) $ do
        text "Greetings, "
        text s

  document :: BL.ByteString -> BL.ByteString
  document cnt =
    [i|<html>
      <head>
        <title>Hyperbole Examples</title>
        <script type="text/javascript" src="/hyperbole.js"></script>
        <style type type="text/css">#{cssResetEmbed}</style>
      </head>
      <body>#{cnt}</body>
    </html>|]

-- document :: BL.ByteString -> BL.ByteString
-- document cnt =
--   [i|<html>
--     <head>
--       <title>Hyperbole Examples</title>
--       <script type="text/javascript">#{scriptEmbed}</script>
--       <style type type="text/css">#{cssResetEmbed}</style>
--     </head>
--     <body>#{cnt}</body>
--   </html>|]
