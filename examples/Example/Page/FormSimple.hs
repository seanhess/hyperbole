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
  deriving (Generic, ViewId)

instance HyperView FormView es where
  data Action FormView
    = Submit
    deriving (Generic, ViewAction)

  update Submit = do
    cf <- formData
    pure $ contactView cf

-- Forms can be pretty simple. Just a type that can be parsed
data ContactForm = ContactForm
  { name :: Text
  , age :: Int
  }
  deriving (Generic, FromForm)

-- and a view that displays an input for each field
formView :: View FormView ()
formView = do
  form Submit (gap 10 . pad 10) $ do
    el Style.h1 "Add Contact"

    -- You must make sure these names match the field names used by FormParse / formData
    field "name" id $ do
      label "Contact Name"
      input Username (inp . placeholder "contact name")

    field "age" id $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")

    submit Style.btn "Submit"
 where
  inp = Style.input

-- Alternatively, use Higher Kinded Types, and Hyperbole can guarantee the field names are the same
--
-- ContactForm' Identity is exactly the same as ContactForm:
-- ContactForm' { name :: Text, age :: Int }
--
-- ContactForm' FieldName:
-- ContactForm' { name :: FieldName Text, age :: FieldName Int }
--
-- ContactForm' Maybe:
-- ContactForm' { name :: Maybe Text, age :: Maybe Int }
--
-- You still have to remember to include all the fields somewhere in the form
data ContactForm' f = ContactForm'
  { name :: Field f Text
  , age :: Field f Int
  }
  deriving (Generic, FromFormF, GenFields FieldName)

formView' :: View FormView ()
formView' = do
  -- generate a ContactForm' FieldName
  let f = fieldNames @ContactForm'
  form Submit (gap 10 . pad 10) $ do
    el Style.h1 "Add Contact"

    -- f.name :: FieldName Text
    -- f.name = FieldName "name"
    field f.name id $ do
      label "Contact Name"
      input Username (inp . placeholder "contact name")

    -- f.age :: FieldName Int
    -- f.age = FieldName "age"
    field f.age id $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")

    submit Style.btn "Submit"
 where
  inp = Style.input

contactView :: ContactForm -> View FormView ()
contactView u = do
  el (bold . Style.success) "Accepted Signup"
  row (gap 5) $ do
    el_ "Username:"
    el_ $ text u.name

  row (gap 5) $ do
    el_ "Age:"
    el_ $ text $ pack (show u.age)
