{-# LANGUAGE QuasiQuotes #-}

module Example.Docs.App where

import Effectful.Dispatch.Dynamic (send)
import Example.Docs.Page.Messages qualified as Messages
import Example.Docs.Page.Users qualified as Users
import Example.Effects.Users (User, Users (..))
import Web.Hyperbole
import Web.Hyperbole.Effect.Response (view)

documentHead :: View DocumentHead ()
documentHead = do
  title "My Website"
  script' scriptEmbed
  style cssEmbed
  script "custom.js"

router :: (Hyperbole :> es) => AppRoute -> Eff es Response
router Messages = runPage Messages.page
router (User cid) = runPage $ Users.page cid
router Main = do
  pure $ view $ do
    el "click a link below to visit a page"
    route Messages "Messages"
    route (User 1) "User 1"
    route (User 2) "User 2"

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

userPage :: (Hyperbole :> es, Users :> es) => Page es '[]
userPage = do
  user <- findUser 100

  -- skipped if user not found
  pure $ userView user

userView :: User -> View c ()
userView _ = none

app :: Application
app = liveApp (document documentHead) (routeRequest router)
