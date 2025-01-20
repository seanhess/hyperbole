module Example.Page.FormSimple where

import Data.Text (Text, pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Style qualified as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[FormView])
page = do
  pure $ exampleLayout Route.FormSimple $ row (pad 20) $ do
    hyper FormView formView

data FormView = FormView
  deriving (Show, Read, ViewId)

instance HyperView FormView es where
  data Action FormView
    = Submit
    deriving (Show, Read, ViewAction)

  update Submit = do
    cf <- formData @ContactForm
    pure $ contactView cf

-- Define a forms as a "Higher Kinded Types"
-- ContactForm Identity behaves just like a simple record: myForm.name == "bob"
-- ContactForm Maybe would make each field (Maybe a)
data ContactForm f = ExampleForm
  { name :: Field f Text
  , age :: Field f Int
  }
  deriving (Generic)
instance Form ContactForm Maybe

formView :: View FormView ()
formView = do
  -- create formfields for our form
  let f = formFields @ContactForm
  form @ContactForm Submit (gap 10 . pad 10) $ do
    el Style.h1 "Add Contact"

    -- pass the form field into the 'field' function
    field f.name (const id) $ do
      label "Contact Name"
      input Username (inp . placeholder "contact name")

    field f.age (const id) $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")

    submit Style.btn "Submit"
 where
  inp = Style.input

contactView :: ContactForm Identity -> View FormView ()
contactView u = do
  el (bold . Style.success) "Accepted Signup"
  row (gap 5) $ do
    el_ "Username:"
    el_ $ text u.name

  row (gap 5) $ do
    el_ "Age:"
    el_ $ text $ pack (show u.age)
