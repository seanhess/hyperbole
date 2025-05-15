module Example.Page.FormSimple where

import Data.Text (Text, pack)
import Example.Style qualified as Style
import Web.Atomic.CSS
import Web.Hyperbole

data AddContact = AddContact
  deriving (Generic, ViewId)

instance HyperView AddContact es where
  data Action AddContact
    = Submit
    deriving (Generic, ViewAction)

  update Submit = do
    cf <- formData
    pure $ contactView cf

-- Forms can be pretty simple. Just a type that can be parsed
data ContactForm = ContactForm
  { name :: Text
  , age :: Int
  , isFavorite :: Bool
  }
  deriving (Generic, FromForm)

-- and a view that displays an input for each field
formView :: View AddContact ()
formView = do
  form Submit ~ gap 15 . pad 10 $ do
    el ~ Style.h1 $ "Add Contact"

    -- Make sure these names match the field names used by FormParse / formData
    field "name" $ do
      label "Contact Name"
      input Username @ placeholder "contact name" ~ Style.input

    field "age" $ do
      label "Age"
      input Number @ placeholder "age" . value "0" ~ Style.input

    field "isFavorite" $ do
      row ~ gap 10 $ do
        checkbox False ~ width 32
        label "Favorite?"

    submit "Submit" ~ Style.btn

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
  , isFavorite :: Field f Bool
  }
  deriving (Generic, FromFormF, GenFields FieldName)

formView' :: View AddContact ()
formView' = do
  -- generate a ContactForm' FieldName
  let f = fieldNames @ContactForm'
  form Submit ~ gap 15 . pad 10 $ do
    el ~ Style.h1 $ "Add Contact"

    -- f.name :: FieldName Text
    -- f.name = FieldName "name"
    field f.name $ do
      label "Contact Name"
      input Username @ placeholder "contact name" ~ Style.input

    -- f.age :: FieldName Int
    -- f.age = FieldName "age"
    field f.age $ do
      label "Age"
      input Number @ placeholder "age" . value "0" ~ Style.input

    field f.isFavorite $ do
      row ~ gap 10 $ do
        checkbox False ~ width 32
        label "Favorite?"

    submit "Submit" ~ Style.btn

contactView :: ContactForm -> View AddContact ()
contactView u = do
  el ~ bold . Style.success $ "Accepted Signup"
  row ~ gap 5 $ do
    el "Username:"
    el $ text u.name

  row ~ gap 5 $ do
    el "Age:"
    el $ text $ pack (show u.age)

  row ~ gap 5 $ do
    el "Favorite:"
    el $ text $ pack (show u.isFavorite)
