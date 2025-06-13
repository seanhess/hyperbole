{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Contact where

import Data.Maybe (fromMaybe)
import Data.String.Conversions
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Dynamic
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), UserId, Users)
import Example.Effects.Users qualified as Users
import Example.Style qualified as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

-- Example adding a reader context to the page, based on an argument from the AppRoute
response :: (Hyperbole :> es, Users :> es, Debug :> es) => UserId -> Eff es Response
response uid = runReader uid $ runPage page

-- The page assumes all effects have been added
page
  :: forall es
   . (Hyperbole :> es, Users :> es, Debug :> es, Reader UserId :> es)
  => Eff es (Page '[Contact])
page = do
  uid <- ask
  u <- Users.find uid
  pure $ exampleLayout (Route.Contacts $ Route.Contact 0) $ do
    col (pad 10 . gap 10) $ do
      hyper (Contact uid) $ contactView u

-- Contact ----------------------------------------------------

data Contact = Contact UserId
  deriving (Generic, ViewId)

instance (Users :> es, Debug :> es) => HyperView Contact es where
  data Action Contact
    = Edit
    | Save
    | View
    deriving (Generic, ViewAction)

  update action = do
    -- No matter which action we are performing, let's look up the user to make sure it exists
    Contact uid <- viewId
    u <- Users.find uid
    case action of
      View -> do
        pure $ contactView u
      Edit -> do
        pure $ contactEditView u
      Save -> do
        delay 1000
        unew <- parseUser uid
        Users.save unew
        pure $ contactView unew

data ContactForm f = ContactForm
  { firstName :: Field f Text
  , lastName :: Field f Text
  , age :: Field f Int
  , info :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Maybe)

parseUser :: (Hyperbole :> es) => Int -> Eff es User
parseUser uid = do
  ContactForm{firstName, lastName, age, info} <- formData @(ContactForm Identity)
  pure User{id = uid, isActive = True, firstName, lastName, age, info}

contactView :: User -> View Contact ()
contactView = contactView' Edit

contactView' :: (ViewId c, ViewAction (Action c)) => Action c -> User -> View c ()
contactView' edit u = do
  col (gap 10) $ do
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
      el id (text "Info:")
      text u.info

    row fld $ do
      el id (text "Active:")
      text (cs $ show u.isActive)

    button edit Style.btn "Edit"
 where
  fld = gap 10

contactEditView :: User -> View Contact ()
contactEditView u = do
  el (hide . onRequest flexCol) contactLoading
  el (onRequest hide) $ contactEdit View Save u

contactEdit :: (ViewId c, ViewAction (Action c)) => Action c -> Action c -> User -> View c ()
contactEdit onView onSave u = do
  contactForm onSave contactFromUser
  col (gap 10) $ do
    button onView Style.btnLight (text "Cancel")
 where
  contactFromUser :: ContactForm Maybe
  contactFromUser =
    ContactForm
      { firstName = Just u.firstName
      , lastName = Just u.lastName
      , age = Just u.age
      , info = Just u.info
      }

contactForm :: (ViewId id, ViewAction (Action id)) => Action id -> ContactForm Maybe -> View id ()
contactForm onSubmit c = do
  let f = fieldNames @ContactForm
  form onSubmit (gap 10) $ do
    field f.firstName fld $ do
      label "First Name:"
      input Name (inp . value (fromMaybe "" c.firstName))

    field f.lastName fld $ do
      label "Last Name:"
      input Name (inp . value (fromMaybe "" c.lastName))

    field f.info fld $ do
      label "Info:"
      textarea (inp . value (fromMaybe "" c.info)) c.info

    field f.age fld $ do
      label "Age:"
      input Number (inp . value (fromMaybe "" $ pack . show <$> c.age))

    submit Style.btn "Submit"
 where
  fld = flexRow . gap 10
  inp = Style.input

contactLoading :: View id ()
contactLoading = el (bg Warning . pad 10) "Loading..."
