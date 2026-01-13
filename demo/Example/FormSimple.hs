{-# LANGUAGE TemplateHaskell #-}

module Example.FormSimple where

import Data.Text (Text, pack)
import App.Docs.Examples
import Example.Style qualified as Style
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

source :: ModuleSource
source = $(moduleSource)

data AddContact = AddContact
  deriving (Generic, ViewId)

instance HyperView AddContact es where
  data Action AddContact
    = Submit
    deriving (Generic, ViewAction)

  update Submit = do
    cf <- formData
    pure $ contactView cf

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  deriving (Generic, FromParam, ToParam, Eq, Show)

data Moon
  = Titan
  | Europa
  | Callisto
  | Mimas
  deriving (Generic, FromParam, ToParam, Eq, Show)

-- Forms can be pretty simple. Just a type that can be parsed
data ContactForm = ContactForm
  { name :: Text
  , age :: Int
  , isFavorite :: Bool
  , planet :: Planet
  , moon :: Moon
  }
  deriving (Generic, FromForm)

-- and a view that displays an input for each field
formView :: View AddContact ()
formView = do
  form Submit ~ gap 15 . pad 10 . flexCol $ do
    el ~ Style.h1 $ "Add Contact"

    -- Make sure these names match the field names used by FormParse / formData
    field "name" $ do
      label $ do
        text "Contact Name"
        input Username @ placeholder "contact name" ~ Style.input

    field "age" $ do
      label $ do
        text "Age"
        input Number @ placeholder "age" . value "0" ~ Style.input

    field "isFavorite" $ do
      label $ do
        row ~ gap 10 $ do
          checkbox False ~ width 32
          text "Favorite?"

    col ~ gap 5 $ do
      el $ text "Planet"
      field "planet" $ do
        radioGroup Earth $ do
          planet Mercury
          planet Venus
          planet Earth
          planet Mars

    field "moon" $ do
      label $ do
        text "Moon"
        select Callisto ~ Style.input $ do
          option Titan "Titan"
          option Europa "Europa"
          option Callisto "Callisto"
          option Mimas "Mimas"

    submit "Submit" ~ btn
 where
  planet val =
    label ~ flexRow . gap 10 $ do
      radio val ~ width 32
      text (pack (show val))

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
  , planet :: Field f Planet
  , moon :: Field f Moon
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
      label $ do
        text "Contact Name"
        input Username @ placeholder "contact name" ~ Style.input

    -- f.age :: FieldName Int
    -- f.age = FieldName "age"
    field f.age $ do
      label $ do
        text "Age"
        input Number @ placeholder "age" . value "0" ~ Style.input

    field f.isFavorite $ do
      label $ do
        row ~ gap 10 $ do
          checkbox False ~ width 32
          text "Favorite?"

    col ~ gap 5 $ do
      el $ text "Planet"
      field f.planet $ do
        radioGroup Earth $ do
          radioOption Mercury
          radioOption Venus
          radioOption Earth
          radioOption Mars

    field f.moon $ do
      label $ do
        text "Moon"
        select Callisto ~ Style.input $ do
          option Titan "Titan"
          option Europa "Europa"
          option Callisto "Callisto"
          option Mimas "Mimas"

    submit "Submit" ~ btn
 where
  radioOption val =
    label ~ flexRow . gap 10 $ do
      radio val ~ width 32
      text (pack (show val))

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

  row ~ gap 5 $ do
    el "Planet:"
    el $ text $ pack (show u.planet)

  row ~ gap 5 $ do
    el "Moon:"
    el $ text $ pack (show u.moon)
