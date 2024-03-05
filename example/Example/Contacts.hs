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
import GHC.Generics (Generic)
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
  deriving (Show, Read, Param)


data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  deriving (Show, Read, Param)


instance HyperView Contacts where
  type Action Contacts = ContactsAction


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
  deriving (Show, Read, Param)


data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, Param)


instance HyperView Contact where
  type Action Contact = ContactAction


data UserForm a = UserForm
  { firstName :: Field a Text
  , lastName :: Field a Text
  , age :: Field a Int
  }
  deriving (Generic, Form)


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
    UserForm{firstName, lastName, age} <- parseForm
    let u' = User{id = u.id, isActive = True, firstName, lastName, age}
    userSave u'
    pure $ contactView u'


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
    form @UserForm Save (pad 10 . gap 10) $ \f -> do
      field fld $ do
        label "First Name:"
        input Name (value u.firstName) f.firstName

      field fld $ do
        label "Last Name:"
        input Name (value u.lastName) f.lastName

      field fld $ do
        label "Age:"
        input Number (value $ cs $ show u.age) f.age

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
