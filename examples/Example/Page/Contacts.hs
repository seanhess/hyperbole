{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Contacts where

import Control.Monad (forM_)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), UserId, Users)
import Example.Effects.Users qualified as Users
import Example.Page.Contact (contactForm, contactLoading, contactView', parseUser)
import Example.Page.Contact qualified as Contact
import Example.Style qualified as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page
  :: forall es
   . (Hyperbole :> es, Users :> es, Debug :> es)
  => Eff es (Page '[Contacts, InlineContact, NewContact])
page = do
  us <- Users.all
  pure $ exampleLayout (Route.Contacts Route.ContactsAll) $ do
    example "Contacts" "Example/Page/Contacts.hs" $ do
      el "This example combines various features"
      col ~ embed $ do
        hyper Contacts $ allContactsView Nothing us

-- Contacts ----------------------------------------------

data Contacts = Contacts
  deriving (Generic, ViewId)

data Filter
  = Active
  | Inactive
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance (Users :> es, Debug :> es) => HyperView Contacts es where
  data Action Contacts
    = Reload (Maybe Filter)
    | AddUser
    | DeleteUser UserId
    deriving (Generic, ViewAction)

  type Require Contacts = '[InlineContact, NewContact]

  update = \case
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
allContactsView fil us = col ~ gap 20 $ do
  row ~ gap 10 $ do
    el ~ pad 10 $ "Filter: "
    dropdown Reload (== fil) $ do
      option Nothing ""
      option (Just Active) "Active!"
      option (Just Inactive) "Inactive"

  row ~ gap 10 $ do
    let filtered = filter (filterUsers fil) us
    forM_ filtered $ \u -> do
      el ~ border 1 . pad 10 $ do
        hyper (InlineContact u.id) $ contactView u
        row $ do
          space
          route (Route.Contacts $ Route.Contact u.id) "details" ~ Style.link

  row ~ gap 10 $ do
    button (Reload Nothing) ~ Style.btnLight $ "Reload"
    target (InlineContact 2) $ button Edit ~ Style.btnLight $ "Edit Sara"

  hyper NewContact newContactButton
 where
  filterUsers Nothing _ = True
  filterUsers (Just Active) u = u.isActive
  filterUsers (Just Inactive) u = not u.isActive

-- New Contact Form / Button ----------------------------------
-- Note that it is easier to nest hyperviews here because NewContact has sufficiently different state
--   * It doesn't need to know the users
--   * It DOES need to track it's open / close state
--   * We use target to submit the form to the Contacts parent view

data NewContact = NewContact
  deriving (Generic, ViewId)

instance (Users :> es) => HyperView NewContact es where
  data Action NewContact
    = ShowForm
    | CloseForm
    deriving (Generic, ViewAction)

  type Require NewContact = '[Contacts]

  update action =
    case action of
      ShowForm -> pure newContactForm
      CloseForm -> pure newContactButton

newContactButton :: View NewContact ()
newContactButton = do
  button ShowForm ~ Style.btn $ "Add Contact"

newContactForm :: View NewContact ()
newContactForm = do
  row ~ pad 10 . gap 10 . border 1 $ do
    target Contacts $ do
      contactForm AddUser genFields
    col $ do
      space
      button CloseForm ~ Style.btnLight $ "Cancel"

-- Reuse Contact View ----------------------------------
-- We want to use the same view as Example.Contact, but customize the edit view to have a delete button
-- Note that we re-implement the actions and the handler
-- Just create functions to deduplicate code and use them here

data InlineContact = InlineContact UserId
  deriving (Generic, ViewId)

instance (Users :> es, Debug :> es) => HyperView InlineContact es where
  data Action InlineContact
    = Edit
    | View
    | Save
    deriving (Generic, ViewAction)

  type Require InlineContact = '[Contacts]

  update a = do
    InlineContact uid <- viewId
    u <- Users.find uid
    case a of
      View ->
        pure $ contactView u
      Edit ->
        pure $ contactEdit u
      Save -> do
        delay 1000
        unew <- parseUser uid
        Users.save unew
        pure $ contactView unew

-- See how we reuse the contactView' from Example.Contact
contactView :: User -> View InlineContact ()
contactView = contactView' Edit

-- See how we reuse the contactEdit' and contactLoading from Example.Contact
contactEdit :: User -> View InlineContact ()
contactEdit u = do
  el ~ (display None . whenLoading flexCol) $ contactLoading
  col ~ (whenLoading (display None) . gap 10) $ do
    Contact.contactEdit View Save u
    target Contacts $ button (DeleteUser u.id) ~ Style.btn' Danger . pad (XY 10 0) $ text "Delete"
