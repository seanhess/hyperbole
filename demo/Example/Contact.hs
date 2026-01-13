{-# LANGUAGE UndecidableInstances #-}

module Example.Contact where

import App.Route (UserId)
import App.Route qualified as Route
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import Data.Text (Text, pack)
import App.Docs.Page
import Effectful
import Effectful.Reader.Dynamic
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users (User (..), Users)
import Example.Effects.Users qualified as Users
import Example.Style qualified as Style
import Example.Style.Cyber (btn)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

-- Example adding a reader context to the page, based on an argument from the AppRoute
response :: (Hyperbole :> es, Users :> es, Debug :> es) => UserId -> Eff es Response
response uid = runReader uid $ runPage page

-- The page assumes all effects have been added
page
  :: forall es
   . (Hyperbole :> es, Users :> es, Debug :> es, Reader UserId :> es)
  => Page es '[Contact]
page = do
  uid <- ask
  u <- Users.find uid
  pure $ layout (Route.Contacts Route.ContactsAll) $ do
    section' "Contact" $ do
      hyper (Contact uid) $ contactView u

-- Contact ----------------------------------------------------

data Contact = Contact UserId
  deriving (Generic, ViewId)

instance (Users :> es, Debug :> es) => HyperView Contact es where
  data Action Contact
    = Edit
    | Save
    | ViewContact
    deriving (Generic, ViewAction)

  update action = do
    -- No matter which action we are performing, let's look up the user to make sure it exists
    Contact uid <- viewId
    u <- Users.find uid
    case action of
      ViewContact -> do
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
  col ~ gap 10 $ do
    row ~ fld $ do
      el (text "First Name:")
      text u.firstName

    row ~ fld $ do
      el (text "Last Name:")
      text u.lastName

    row ~ fld $ do
      el (text "Age:")
      text (cs $ show u.age)

    row ~ fld $ do
      el (text "Info:")
      text u.info

    row ~ fld $ do
      el (text "Active:")
      text (cs $ show u.isActive)

    button edit "Edit" ~ btn
 where
  fld = gap 10

contactEditView :: User -> View Contact ()
contactEditView u = do
  el contactLoading ~ display None . whenLoading flexCol
  el (contactEdit ViewContact Save u) ~ whenLoading (display None)

contactEdit :: (ViewId c, ViewAction (Action c)) => Action c -> Action c -> User -> View c ()
contactEdit onView onSave u = do
  col ~ gap 10 $ do
    contactForm onSave contactFromUser
    button onView (text "Cancel") ~ Style.btnLight
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
  form onSubmit ~ gap 10 $ do
    field f.firstName ~ fld $ do
      label $ do
        text "First Name:"
        input Name @ value (fromMaybe "" c.firstName) ~ Style.input

    field f.lastName ~ fld $ do
      label $ do
        text "Last Name:"
        input Name @ value (fromMaybe "" c.lastName) ~ Style.input

    field f.info ~ fld $ do
      label $ do
        text "Info:"
        textarea c.info @ value (fromMaybe "" c.info) ~ Style.input

    field f.age ~ fld $ do
      label $ do
        text "Age:"
        input Number @ value (maybe "" (pack . show) c.age) ~ inp

    submit "Submit" ~ btn
 where
  fld :: (Styleable a) => CSS a -> CSS a
  fld = flexRow . gap 10
  inp = Style.input

contactLoading :: View id ()
contactLoading = el ~ (bg Warning . pad 10) $ "Loading..."
