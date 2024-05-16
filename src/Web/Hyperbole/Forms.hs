{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Forms
  ( FormFields (..)
  , InputType (..)
  , Label
  , Invalid
  , Input
  , InputName (..)
  , field
  , label
  , input
  , form
  , submit
  , parseForm
  , Form (..)
  , Field
  , defaultFormOptions
  , FormOptions (..)
  , genericFromForm
  , (<?>)
  , (<|>)
  )
where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView (HyperView (..), Param (..), dataTarget)
import Web.Internal.FormUrlEncoded (FormOptions (..), GFromForm, defaultFormOptions, genericFromForm)
import Web.View hiding (form, input, label)


-- | The only time we can use Fields is inside a form
newtype FormFields f id = FormFields id
  deriving newtype (Show)


instance (Param id, Show id) => Param (FormFields f id) where
  parseParam t = FormFields <$> parseParam t
  toParam (FormFields i) = toParam i


instance (HyperView id, Show id) => HyperView (FormFields f id) where
  type Action (FormFields f id) = Action id


-- | TODO: there are many more of these: https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete
data InputType
  = NewPassword
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


data Label a


data Invalid a


data Input a = Input InputName


newtype InputName = InputName Text


inputName :: String -> InputName
inputName = InputName . pack


field :: Mod -> InputName -> View (Input id) () -> View (FormFields form id) ()
field f inp cnt =
  tag "label" (f . flexCol) $
    addContext (Input inp) cnt


label :: Text -> View (Input id) ()
label = text


input :: InputType -> Mod -> View (Input id) ()
input ft f = do
  Input (InputName nm) <- context
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


(<?>) :: (c -> View id ()) -> Maybe c -> View id ()
_ <?> Nothing = none
vf <?> (Just c) = vf c
infixl 3 <?>


form :: forall form id. (Form form, HyperView id) => Action id -> Mod -> (form Label -> View (FormFields form id) ()) -> View id ()
form a f fcnt = do
  vid <- context
  let frm = formLabels :: form Label
  let cnt = fcnt frm
  tag "form" (onSubmit a . dataTarget vid . f . flexCol) $ addContext (FormFields vid) cnt
 where
  onSubmit :: (Param a) => a -> Mod
  onSubmit = att "data-on-submit" . toParam


submit :: Mod -> View (FormFields form id) () -> View (FormFields form id) ()
submit f = tag "button" (att "type" "submit" . f)


type family Field (context :: Type -> Type) a
type instance Field Identity a = a
type instance Field Label a = InputName
type instance Field Invalid a = Maybe Text


parseForm :: forall form es. (Form form, Hyperbole :> es) => Eff es (form Identity)
parseForm = do
  f <- formData
  let ef = fromForm f :: Either Text (form Identity)
  either parseError pure ef


class Form (form :: (Type -> Type) -> Type) where
  formLabels :: form Label
  default formLabels :: (Generic (form Label), GForm (Rep (form Label))) => form Label
  formLabels = to gForm


  formInvalid :: form Invalid
  default formInvalid :: (Generic (form Invalid), GForm (Rep (form Invalid))) => form Invalid
  formInvalid = to gForm


  -- formFields :: [form a]
  -- formFields

  fromForm :: FE.Form -> Either Text (form Identity)
  default fromForm :: (Generic (form Identity), GFromForm (form Identity) (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
  fromForm = genericFromForm defaultFormOptions


-- | Automatically derive labels from form field names
class GForm f where
  gForm :: f p


instance GForm U1 where
  gForm = U1


instance (GForm f, GForm g) => GForm (f :*: g) where
  gForm = gForm :*: gForm


instance (GForm f) => GForm (M1 D d f) where
  gForm = M1 gForm


instance (GForm f) => GForm (M1 C c f) where
  gForm = M1 gForm


instance (Selector s) => GForm (M1 S s (K1 R InputName)) where
  gForm = M1 . K1 $ inputName (selName (undefined :: M1 S s (K1 R Text) p))


instance GForm (M1 S s (K1 R (Maybe Text))) where
  gForm = M1 . K1 $ Nothing
