module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), Users)
import Example.Effects.Users qualified as Users
import Web.Hyperbole

page :: forall es. (Hyperbole :> es, Users :> es, Debug :> es) => Page es ()
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
  deriving (Show, Read, Param, HyperView ContactsAction)

data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  deriving (Show, Read, Param)

data Filter
  = Active
  | Inactive
  deriving (Show, Read, Eq)

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
    button (Reload Nothing) (bg GrayLight) "Reload"

    dropdown Reload (== fil) $ do
      option Nothing id ""
      option (Just Active) id "Active!"
      option (Just Inactive) id "Inactive"

    target (Contact 2) $ button Edit (bg GrayLight) "Edit 2"

  row (pad 10 . gap 10) $ do
    let filtered = filter (filterUsers fil) us
    forM_ filtered $ \u -> do
      el (border 1) $ do
        viewId (Contact u.id) $ contactView u
 where
  filterUsers Nothing _ = True
  filterUsers (Just Active) u = u.isActive
  filterUsers (Just Inactive) u = not u.isActive

-- Contact ----------------------------------------------------

data Contact = Contact Int
  deriving (Show, Read, Param, HyperView ContactAction)

data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, Param)

contact :: (Hyperbole :> es, Users :> es, Debug :> es) => Contact -> ContactAction -> Eff es (View Contact ())
contact (Contact uid) a = do
  u <- userFind uid
  action u a
 where
  action u View = do
    pure $ contactView u
  action u Edit = do
    pure $ contactEdit u
  action u Save = do
    delay 1000
    u' <- userFormData u.id
    userSave u'
    pure $ contactView u'

contactView :: User -> View Contact ()
contactView u = do
  col (pad 10 . gap 10) $ do
    el_ $ do
      label id (text "First Name:")
      text u.firstName

    el_ $ do
      label id (text "Last Name:")
      text u.lastName

    el_ $ do
      label id (text "Age:")
      text (cs $ show u.age)

    el_ $ do
      label id (text "Active:")
      text (cs $ show u.isActive)

    button Edit (bg Primary . color White . hover (bg PrimaryLight . color Dark)) "Edit"

contactEdit :: User -> View Contact ()
contactEdit u =
  onRequest loading $ do
    form Save (pad 10 . gap 10) $ do
      label id $ do
        text "First Name"
        input (name "firstName" . value u.firstName)

      label id $ do
        text "Last Name"
        input (name "lastName" . value u.lastName)

      label id $ do
        text "Age"
        input (name "age" . value (cs $ show u.age))

      submit id "Submit"

      button View id (text "Cancel")

      target Contacts $ button (Delete u.id) (bg Secondary) (text "Delete")
 where
  loading = el (bg Secondary) "Loading..."

userFormData :: (Hyperbole :> es) => Int -> Eff es User
userFormData uid = do
  f <- formData
  firstName <- param "firstName" f
  lastName <- param "lastName" f
  age <- param "age" f
  pure $ User uid firstName lastName age True

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
