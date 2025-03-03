{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.View.Form where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Text.Casing (kebab)
import Web.Hyperbole.Data.Form (FieldName (..), Validated (..))
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Event (onSubmit)
import Web.View hiding (form, input, label)
import Web.View.Style (addClass, cls, prop)


-- | Context that allows form fields
data FormFields id = FormFields id


{- | Type-safe \<form\>. Calls (Action id) on submit

@
#EMBED Example/Page/FormSimple.hs formView
@
-}
form :: (ViewAction (Action id)) => Action id -> Mod id -> View (FormFields id) () -> View id ()
form a md cnt = do
  vid <- context
  tag "form" (onSubmit a . md . flexCol . marginEnd0) $ do
    addContext (FormFields vid) cnt
 where
  -- not sure why chrome is adding margin-block-end: 16 to forms? Add to web-view?
  marginEnd0 =
    addClass $
      cls "mg-end-0"
        & prop @PxRem "margin-block-end" 0


-- | Button that submits the 'form'
submit :: Mod (FormFields id) -> View (FormFields id) () -> View (FormFields id) ()
submit f = tag "button" (att "type" "submit" . f)


-- | Display a 'FormField'. See 'form' and 'Form'
field
  :: forall (id :: Type) (a :: Type)
   . FieldName a
  -> Mod (FormFields id)
  -> View (Input id a) ()
  -> View (FormFields id) ()
field fn f inputs = do
  tag "label" (f . flexCol) $ do
    addContext (Input fn) inputs


-- | Choose one for 'input's to give the browser autocomplete hints
data InputType
  = -- TODO: there are many more of these: https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete
    NewPassword
  | CurrentPassword
  | Username
  | Email
  | Number
  | TextInput
  | Name
  | OneTimeCode
  | Organization
  | StreetAddress
  | Country
  | CountryName
  | PostalCode
  | Search
  deriving (Show)


data Input (id :: Type) (a :: Type) = Input
  { inputName :: FieldName a
  }


-- | label for a 'field'
label :: Text -> View (Input id a) ()
label = text


-- | input for a 'field'
input :: InputType -> Mod (Input id a) -> View (Input id a) ()
input ft f = do
  Input (FieldName nm) <- context
  tag "input" (f . name nm . att "type" (inpType ft) . att "autocomplete" (auto ft)) none
 where
  inpType NewPassword = "password"
  inpType CurrentPassword = "password"
  inpType Number = "number"
  inpType Email = "email"
  inpType Search = "search"
  inpType _ = "text"

  auto :: InputType -> Text
  auto = pack . kebab . show


placeholder :: Text -> Mod id
placeholder = att "placeholder"


-- | textarea for a 'field'
textarea :: Mod (Input id a) -> Maybe Text -> View (Input id a) ()
textarea f mDefaultText = do
  Input (FieldName nm) <- context
  tag "textarea" (f . name nm) (text $ fromMaybe "" mDefaultText)


{- Only shows if 'Validated' is 'Invalid'. See 'formFieldsWith'formform
@
@
-}
invalidText :: forall a id. Validated a -> View (Input id a) ()
invalidText v = do
  case v of
    Invalid t -> text t
    _ -> none
