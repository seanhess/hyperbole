module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), Users)
import Example.Effects.Users qualified as Users
import Example.Style qualified as Style
import Web.Hyperbole


page
  :: forall es
   . (Hyperbole :> es, Users :> es, Debug :> es)
  => Page es '[Contacts, Contact]
page = do
  handle contacts $ handle contact $ load $ do
    us <- usersAll
    pure $ do
      col (pad 10 . gap 10) $ do
        hyper Contacts $ allContactsView Nothing us


-- Contacts ----------------------------------------------

data Contacts = Contacts
  deriving (Show, Read, ViewId)


data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  | AddUser
  deriving (Show, Read, ViewAction)


data Filter
  = Active
  | Inactive
  deriving (Eq, Show, Read)


instance HyperView Contacts where
  type Action Contacts = ContactsAction
  type Require Contacts = '[Contact]


contacts :: (Hyperbole :> es, Users :> es, Debug :> es) => Contacts -> ContactsAction -> Eff es (View Contacts ())
contacts _ (Reload mf) = do
  us <- usersAll
  pure $ allContactsView mf us
contacts _ (Delete uid) = do
  userDelete uid
  us <- usersAll
  pure $ allContactsView Nothing us
contacts _ AddUser = do
  uid <- usersNextId
  u <- parseUser uid
  userSave u
  us <- usersAll
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
      el (border 1) $ do
        hyper (Contact u.id) $ contactView u

  row (gap 10) $ do
    button (Reload Nothing) Style.btnLight "Reload"
    target (Contact 2) $ button Edit Style.btnLight "Edit Sara"

  el bold "Add Contact"

  row (pad 10 . gap 10 . border 1) $ do
    contactForm AddUser genForm
 where
  filterUsers Nothing _ = True
  filterUsers (Just Active) u = u.isActive
  filterUsers (Just Inactive) u = not u.isActive


-- Contact ----------------------------------------------------

data Contact = Contact Int
  deriving (Show, Read, ViewId)


data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, ViewAction)


instance HyperView Contact where
  type Action Contact = ContactAction


data ContactForm f = ContactForm
  { firstName :: Field f Text
  , lastName :: Field f Text
  , age :: Field f Int
  }
  deriving (Generic)
instance Form ContactForm Maybe


contactFromUser :: User -> ContactForm Maybe
contactFromUser u =
  ContactForm
    { firstName = Just u.firstName
    , lastName = Just u.lastName
    , age = Just u.age
    }


contact :: (Hyperbole :> es, Users :> es, Debug :> es) => Contact -> ContactAction -> Eff es (View Contact ())
contact (Contact uid) a = do
  -- Lookup the user in the database for all actions
  u <- userFind uid
  action u a
 where
  action u View = do
    pure $ contactView u
  action u Edit = do
    pure $ contactEdit u
  action _ Save = do
    delay 1000
    unew <- parseUser uid
    userSave unew
    pure $ contactView unew


parseUser :: (Hyperbole :> es) => Int -> Eff es User
parseUser uid = do
  ContactForm{firstName, lastName, age} <- formData @ContactForm
  pure User{id = uid, isActive = True, firstName, lastName, age}


contactView :: User -> View Contact ()
contactView u = do
  col (pad 10 . gap 10) $ do
    row fld $ do
      el id (text "First Name:")
      text u.firstName

    row fld $ do
      el id (text "Last Name:")
      text u.lastName

    row fld $ do
      el id (text "Age:")
      text (cs $ show u.age)

    row fld $ do
      el id (text "Active:")
      text (cs $ show u.isActive)

    button Edit Style.btn "Edit"
 where
  fld = gap 10


contactEdit :: User -> View Contact ()
contactEdit u = do
  onRequest loading $ do
    col (gap 10 . pad 10) $ do
      contactForm Save (contactFromUser u)
      button View Style.btnLight (text "Cancel")
      target Contacts $ button (Delete u.id) (Style.btn' Danger) (text "Delete")
 where
  loading = el (bg Warning . pad 10) "Loading..."


contactForm :: (HyperView id) => Action id -> ContactForm Maybe -> View id ()
contactForm onSubmit c = do
  let f = genFieldsWith c
  form @ContactForm onSubmit (gap 10) $ do
    field f.firstName (const fld) $ do
      label "First Name:"
      input Name (inp . valMaybe id c.firstName)

    field f.lastName (const fld) $ do
      label "Last Name:"
      input Name (inp . valMaybe id c.lastName)

    field f.age (const fld) $ do
      label "Age:"
      input Number (inp . valMaybe (pack . show) c.age)

    submit Style.btn "Submit"
 where
  fld = flexRow . gap 10
  inp = Style.input
  valMaybe _ Nothing = id
  valMaybe f (Just a) = value (f a)


userFind :: (Hyperbole :> es, Users :> es) => Int -> Eff es User
userFind uid = do
  mu <- send (Users.LoadUser uid)
  maybe notFound pure mu


usersAll :: (Users :> es) => Eff es [User]
usersAll = send Users.LoadUsers


userSave :: (Users :> es) => User -> Eff es ()
userSave = send . Users.SaveUser


userDelete :: (Users :> es) => Int -> Eff es ()
userDelete = send . Users.DeleteUser


usersNextId :: (Users :> es) => Eff es Int
usersNextId = send Users.NextId
