{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Forms where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView (HyperView (..), Param (..), dataTarget)
import Web.Internal.FormUrlEncoded (GFromForm, defaultFormOptions, genericFromForm)
import Web.View hiding (form, input)


-- | The only time we can use Fields is inside a form
newtype FormFields f id = FormFields id
  deriving newtype (Show)


instance (Param id, Show id) => Param (FormFields f id) where
  parseParam t = FormFields <$> parseParam t
  toParam (FormFields i) = toParam i


instance (HyperView id, Show id) => HyperView (FormFields f id) where
  type Action (FormFields f id) = Action id


-- | TODO: there are many more of these: https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete
data FieldInput
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


data Input a = Input


newtype InputName = InputName Text


field :: Mod -> View (Input id) () -> View (FormFields form id) ()
field f cnt =
  tag "label" (f . flexCol)
    $ addContext Input cnt


label :: Text -> View (Input id) ()
label = text


input :: FieldInput -> Mod -> InputName -> View (Input id) ()
input fi f (InputName n) = tag "input" (f . name n . att "type" (typ fi) . att "autocomplete" (auto fi)) none
 where
  typ NewPassword = "password"
  typ CurrentPassword = "password"
  typ Number = "number"
  typ Email = "email"
  typ Search = "search"
  typ _ = "text"

  auto :: FieldInput -> Text
  auto = pack . kebab . show


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


parseForm :: forall form es. (Form form, Hyperbole :> es) => Eff es (form Identity)
parseForm = do
  (f :: FE.Form) <- formData
  let ef = fromForm f :: Either Text (form Identity)
  either parseError pure ef


class Form (form :: (Type -> Type) -> Type) where
  formLabels :: form Label
  default formLabels :: (Generic (form Label), GFormLabels (Rep (form Label))) => form Label
  formLabels = to gFormLabels


  fromForm :: FE.Form -> Either Text (form Identity)
  default fromForm :: (Generic (form Identity), GFromForm (form Identity) (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
  fromForm = genericFromForm defaultFormOptions


type family Field (context :: Type -> Type) a
type instance Field Identity a = a
type instance Field Label a = InputName


-- | Automatically derive labels from form field names
class GFormLabels f where
  gFormLabels :: f p


instance GFormLabels U1 where
  gFormLabels = U1


instance (GFormLabels f, GFormLabels g) => GFormLabels (f :*: g) where
  gFormLabels = gFormLabels :*: gFormLabels


instance (Selector s) => GFormLabels (M1 S s (K1 R InputName)) where
  gFormLabels = M1 . K1 $ InputName $ pack (selName (undefined :: M1 S s (K1 R Text) p))


instance (GFormLabels f) => GFormLabels (M1 D d f) where
  gFormLabels = M1 gFormLabels


instance (GFormLabels f) => GFormLabels (M1 C c f) where
  gFormLabels = M1 gFormLabels
