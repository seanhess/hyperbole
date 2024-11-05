{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Data.Text (Text, pack)
import Debug.Trace
import Effectful
import Example.AppRoute qualified as AppRoute
import Example.Colors
import Example.Contact
import Example.Effects.Debug
import Example.Effects.Users (User (..), UserId, Users)
import Example.Effects.Users qualified as Users
import Example.Style qualified as Style
import Web.Hyperbole
import Web.Hyperbole.Handler


page
  :: forall es
   . (Hyperbole :> es, Users :> es, Debug :> es)
  => Page es '[Contacts, InlineContact]
page = do
  us <- Users.all
  pure $ do
    col (pad 10 . gap 10) $ do
      hyper Contacts $ allContactsView Nothing us


-- Contacts ----------------------------------------------

data Contacts = Contacts
  deriving (Show, Read, ViewId)


data ContactsAction
  = Reload (Maybe Filter)
  | AddUser
  | DeleteUser UserId
  deriving (Show, Read, ViewAction)


data Filter
  = Active
  | Inactive
  deriving (Eq, Show, Read)


instance HyperView Contacts where
  type Action Contacts = ContactsAction
  type Require Contacts = '[InlineContact]
instance (Users :> es, Debug :> es) => Handle Contacts es where
  handle = \case
    Reload mf -> do
      us <- Users.all
      pure $ allContactsView mf us
    AddUser -> do
      uid <- Users.nextId
      u <- parseUser uid
      Users.save u
      us <- Users.all
      pure $ allContactsView Nothing us
    DeleteUser uid -> do
      Users.delete uid
      us <- Users.all
      pure $ allContactsView Nothing us


-- TODO: get the form to close when submitted

allContactsView :: Maybe Filter -> [User] -> View Contacts ()
allContactsView fil us = col (gap 20) $ do
  row (gap 10) $ do
    el (pad 10) "Filter: "
    dropdown Reload (== fil) id $ do
      option Nothing ""
      option (Just Active) "Active!"
      option (Just Inactive) "Inactive"

  row (gap 10) $ do
    let filtered = filter (filterUsers fil) us
    forM_ filtered $ \u -> do
      el (border 1 . pad 10) $ do
        hyper (InlineContact $ Contact u.id) $ contactView' u
        row id $ do
          space
          link (routeUrl $ AppRoute.Contacts $ AppRoute.Contact u.id) Style.link "details"

  row (gap 10) $ do
    button (Reload Nothing) Style.btnLight "Reload"
    target (Contact 2) $ button Edit Style.btnLight "Edit Sara"

  el bold "Add Contact"

  row (pad 10 . gap 10 . border 1) $ do
    contactForm AddUser (ContactForm Nothing Nothing Nothing)
 where
  filterUsers Nothing _ = True
  filterUsers (Just Active) u = u.isActive
  filterUsers (Just Inactive) u = not u.isActive

  contactView' :: User -> View InlineContact ()
  contactView' u = do
    InlineContact c <- viewId
    addContext c $ contactView u


-- Reuse Contact View ----------------------------------

-- Make a newtype so we can customize the behavior of the handler
newtype InlineContact = InlineContact {contact :: Contact}
  deriving newtype (ViewId)


instance HyperView InlineContact where
  type Action InlineContact = ContactAction
instance (Users :> es, Debug :> es) => Handle InlineContact es where
  handle Edit = do
    -- Edit will show the normal view, plus a delete button
    InlineContact (Contact uid) <- viewId
    u <- Users.find uid
    pure $ inlineEdit u
  handle other = do
    delegate (.contact) other


inlineEdit :: User -> View InlineContact ()
inlineEdit u = onRequest contactLoading $ col (gap 10) $ do
  mapView (.contact) $ contactEdit' u
  target Contacts $ button (DeleteUser u.id) (Style.btn' Danger . pad (XY 10 0)) (text "Delete")
