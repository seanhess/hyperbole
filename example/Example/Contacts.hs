module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), Users)
import Example.Effects.Users qualified as Users
import Example.Style as Style
import Web.Hyperbole


page :: forall es. (Hyperbole :> es, Users :> es, Debug :> es) => Page es Response
page = do
  hyper contacts
  hyper contact
  load $ do
    us <- usersAll
    pure $ do
      col (pad 10 . gap 10) $ do
        viewId Contacts $ allContactsView Nothing us


-- Contacts ----------------------------------------------

data Contacts = Contacts
  deriving (Generic, Param)


data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  deriving (Generic, Param)


data Filter
  = Active
  | Inactive
  deriving (Eq, Generic, Param)


instance HyperView Contacts where
  type Action Contacts = ContactsAction


contacts :: (Hyperbole :> es, Users :> es, Debug :> es) => Contacts -> ContactsAction -> Eff es (View Contacts ())
contacts _ (Reload mf) = do
  us <- usersAll
  pure $ allContactsView mf us
contacts _ (Delete uid) = do
  userDelete uid
  us <- usersAll
  pure $ allContactsView Nothing us


allContactsView :: Maybe Filter -> [User] -> View Contacts ()
allContactsView fil us = do
  row (gap 10) $ do
    el (pad 10) "Filter: "
    dropdown Reload (== fil) id $ do
      option Nothing ""
      option (Just Active) "Active!"
      option (Just Inactive) "Inactive"

  row (pad 10 . gap 10) $ do
    let filtered = filter (filterUsers fil) us
    forM_ filtered $ \u -> do
      el (border 1) $ do
        viewId (Contact u.id) $ contactView u

  row (gap 10) $ do
    button (Reload Nothing) Style.btnLight "Reload"
    target (Contact 2) $ button Edit Style.btnLight "Edit Sara"
 where
  filterUsers Nothing _ = True
  filterUsers (Just Active) u = u.isActive
  filterUsers (Just Inactive) u = not u.isActive


-- Contact ----------------------------------------------------

data Contact = Contact Int
  deriving (Generic, Param)


data ContactAction
  = Edit
  | Save
  | View
  deriving (Generic, Param)


instance HyperView Contact where
  type Action Contact = ContactAction


-- Form Fields
data FirstName = FirstName Text deriving (Generic, FormField)
data LastName = LastName Text deriving (Generic, FormField)
data Age = Age Int deriving (Generic, FormField)


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
    unew <- parseUser
    userSave unew
    pure $ contactView unew

  parseUser = do
    FirstName firstName <- formField @FirstName
    LastName lastName <- formField @LastName
    Age age <- formField @Age
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
contactEdit u =
  onRequest loading $ do
    form Save mempty (pad 10 . gap 10) $ do
      field @FirstName fld id $ do
        label "First Name:"
        input Name (value u.firstName)

      field @LastName fld id $ do
        label "Last Name:"
        input Name (value u.lastName)

      field @Age fld id $ do
        label "Age:"
        input Number (value $ cs $ show u.age)

      submit Style.btn "Submit"

      button View Style.btnLight (text "Cancel")

      target Contacts $ button (Delete u.id) (Style.btn' Danger) (text "Delete")
 where
  loading = el (bg Warning . pad 10) "Loading..."
  fld = flexRow . gap 10


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
