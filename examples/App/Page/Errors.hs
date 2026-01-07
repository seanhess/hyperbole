{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Errors where

import App.Route as Route hiding (UserId)
import Control.Monad (forM_)
import Data.List qualified as L
import Data.Text (Text, pack, unpack)
import Docs.Examples
import Docs.Page
import Effectful
import Effectful.Exception
import Example.Colors
import Example.Style.Cyber (btn)
import Example.View.Layout
import Text.Read (readMaybe)
import Web.Atomic.CSS
import Web.Hyperbole hiding (link)

page :: (Hyperbole :> es) => Page es '[Defaults, Users]
page = do
  pure $ layout Errors $ do
    section' "Exceptions" $ do
      el "Any uncaught exceptions thrown from a handler will be displayed in a bright red box inline in the corresponding HyperView"
      example source $ do
        hyper Exceptions viewExceptions

    section' "Edge Cases" $ do
      el "You can use the same mechanism to exit execution early and display an application error to handle edge cases"
      example source $ do
        hyper KnownUsers viewKnownUsers

    section' "Handling in Views" $ do
      el "Handle any expected errors in your view function, by making it accept a Maybe or Either"
      example source $ do
        hyper SearchUsers viewSearchUsers

    section' "Custom Error Views" $ do
      el "You can also exit execution early and display a custom view from application code or from caught execptions"
      example source $ do
        hyper Customs viewCustom
 where
  source = $(moduleSource)

-- Defaults ------------------------------------------------

data Defaults = Exceptions | Customs
  deriving (Generic, ViewId)

instance HyperView Defaults es where
  data Action Defaults
    = CauseServerside
    | CauseUserFacing
    | CauseCustom
    deriving (Generic, ViewAction)

  update CauseServerside = do
    _ <- throwIO $ SomeServerError "Oh no!"
    pure $ el "unreachable"
  update CauseUserFacing = do
    _ <- respondError "This is a user-facing custom error"
    pure $ el "unreachable"
  update CauseCustom = do
    _ <- respondErrorView "Something" $ do
      el ~ border 1 . borderColor Danger . rounded 3 $ "Style errors however you want!"
    pure $ el "unreachable"

viewExceptions :: View Defaults ()
viewExceptions = do
  row ~ gap 10 $ do
    button CauseServerside ~ btn $ "Cause Exception"

viewCustom :: View Defaults ()
viewCustom = do
  row ~ gap 10 $ do
    button CauseUserFacing ~ btn $ "Custom Error Message"
    button CauseCustom ~ btn $ "Custom Error View"

-- Users ------------------------------------------------

data User = User
  { id :: Int
  , username :: Text
  }

type UserId = Int
type UserName = Text

fakeDatabase :: [User]
fakeDatabase =
  [ User 1 "Bob"
  , User 2 "Sarah"
  , User 3 "Alice"
  ]

findUser :: UserId -> Eff es (Maybe User)
findUser uid =
  pure $ L.find (\(User i _) -> uid == i) fakeDatabase

-- KnownUsers ------------------------------------------------

data Users = KnownUsers | SearchUsers
  deriving (Generic, ViewId)

instance HyperView Users es where
  data Action Users
    = UserDetails Int
    | SearchUser Text
    deriving (Generic, ViewAction)

  update (UserDetails uid) = do
    mu <- findUser uid
    case mu of
      Nothing -> notFound
      Just u -> pure $ do
        viewWithDetails (viewUserDetails u) viewKnownUsers
  update (SearchUser term) = do
    mu <- searchUser term
    pure $ do
      viewWithDetails (viewSearchResults mu) viewSearchUsers

viewKnownUsers :: View Users ()
viewKnownUsers = do
  col ~ gap 10 $ do
    el "We know all these users exist when the view was rendered, so one going missing is unlikely"
    row ~ gap 10 $ do
      forM_ fakeDatabase $ \u -> do
        button (UserDetails u.id) ~ btn $ text $ "User: " <> pack (show u.id)

    el "If a user were deleted between when they were rendered and loaded, the error would look like this:"
    button (UserDetails 4) ~ btn $ "Attempt to load non-existing User 4"

viewWithDetails :: View c () -> View c () -> View c ()
viewWithDetails details cnt = do
  col ~ gap 10 $ do
    details
    cnt

viewUserDetails :: User -> View c ()
viewUserDetails u = do
  col ~ gap 10 . pad 10 . border 1 $ do
    el $ do
      text "ID: "
      text $ pack $ show u.id
    el $ do
      text "Name: "
      text u.username

-- SearchUsers ------------------------------------------------

searchUser :: Text -> Eff es (Maybe User)
searchUser searchTerm =
  pure $ findId searchTerm
 where
  findId term = do
    uid <- readMaybe @Int (unpack term)
    L.find (\(User i _) -> uid == i) fakeDatabase

viewSearchUsers :: View Users ()
viewSearchUsers = do
  el "Search for a user by id"
  search SearchUser 250 ~ border 1 . pad 10 @ placeholder "2"

viewSearchResults :: Maybe User -> View c ()
viewSearchResults mu = do
  case mu of
    Nothing -> el ~ italic $ "User not found. No big deal. Doesn't need to be an application error"
    Just u -> viewUserDetails u

-----------------------------------------------------------

data SomeServerError
  = SomeServerError String
  deriving (Show, Eq, Exception)
