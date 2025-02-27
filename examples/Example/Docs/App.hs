{-# LANGUAGE QuasiQuotes #-}

module Example.Docs.App where

import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic (send)
import Example.Effects.Users (User, Users (..))

import Example.Docs.Page.Messages qualified as Messages
import Example.Docs.Page.Users qualified as Users
import Web.Hyperbole

customDocument :: ByteString -> ByteString
customDocument content =
  [i|<html>
    <head>
      <title>My Website</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type="text/css">#{cssResetEmbed}</style>
      <script type="text/javascript" src="custom.js"></script>
    </head>
    <body>#{content}</body>
  </html>|]

router :: (Hyperbole :> es) => AppRoute -> Eff es Response
router Messages = runPage Messages.page
router (User cid) = runPage $ Users.page cid
router Main = do
  view $ do
    el_ "click a link below to visit a page"
    route Messages id "Messages"
    route (User 1) id "User 1"
    route (User 2) id "User 2"

type UserId = Int

data AppRoute
  = Main
  | Messages
  | User UserId
  deriving (Eq, Generic)

instance Route AppRoute where
  baseRoute = Just Main

findUser :: (Hyperbole :> es, Users :> es) => Int -> Eff es User
findUser uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu

userPage :: (Hyperbole :> es, Users :> es) => Eff es (Page '[])
userPage = do
  user <- findUser 100

  -- skipped if user not found
  pure $ userView user

userView :: User -> View c ()
userView _ = none
