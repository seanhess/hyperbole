{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Forms
  ( FormFields (..)
  , InputType (..)
  , Label
  , Invalid
  , Input (..)
  , field
  , label
  , input
  , form
  , submit
  , parseForm
  , formField
  , Form (..)
  , defaultFormOptions
  , FormOptions (..)
  , genericFromForm
  , FromHttpApiData
  , Validation (..)
  , FormField (..)
  , lookupInvalid
  , invalidStyle
  , invalidText
  , validate
  , validation
  , Generic
  )
where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Text
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.FormUrlEncoded qualified as FE
import Web.HttpApiData (FromHttpApiData (..))
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView (HyperView (..), Param (..), dataTarget)
import Web.Internal.FormUrlEncoded (FormOptions (..), GFromForm, defaultFormOptions, genericFromForm)
import Web.View hiding (form, input, label)


-- | The only time we can use Fields is inside a form
data FormFields id = FormFields id Validation


instance (Show id) => Show (FormFields id) where
  show (FormFields i _) = show i


instance (Param id, Show id) => Param (FormFields id) where
  parseParam t = do
    i <- parseParam t
    pure $ FormFields i mempty
  toParam (FormFields i _) = toParam i


instance (HyperView id, Show id) => HyperView (FormFields id) where
  type Action (FormFields id) = Action id


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


newtype Validation = Validation [(Text, Text)]
  deriving newtype (Semigroup, Monoid)


validation :: [Maybe (Text, Text)] -> Validation
validation = Validation . catMaybes


invalidStyle :: forall a. (FormField a) => Mod -> Validation -> Mod
invalidStyle f errs =
  case lookupInvalid @a errs of
    Nothing -> id
    Just _ -> f


lookupInvalid :: forall a. (FormField a) => Validation -> Maybe Text
lookupInvalid (Validation es) = lookup (inputName @a) es


invalidText :: forall a id. (FormField a) => View (Input id a) ()
invalidText = do
  Input _ v <- context
  maybe none text $ lookupInvalid @a v


validate :: forall a. (FormField a) => Bool -> Text -> Maybe (Text, Text)
validate True t = Just (inputName @a, t)
validate False _ = Nothing


data Label a


data Invalid a


data Input id a = Input Text Validation


field :: forall a id. (FormField a) => Mod -> Mod -> View (Input id a) () -> View (FormFields id) ()
field f inv cnt = do
  let n = inputName @a
  FormFields _ v <- context
  tag "label" (f . flexCol . invalidStyle @a inv v) $
    addContext (Input n v) cnt


label :: Text -> View (Input id a) ()
label = text


input :: InputType -> Mod -> View (Input id a) ()
input ft f = do
  Input nm _ <- context
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


form :: forall id. (HyperView id) => Action id -> Validation -> Mod -> View (FormFields id) () -> View id ()
form a v f cnt = do
  vid <- context
  -- let frm = formLabels :: form Label
  -- let cnt = fcnt frm
  tag "form" (onSubmit a . dataTarget vid . f . flexCol) $ addContext (FormFields vid v) cnt
 where
  onSubmit :: (Param a) => a -> Mod
  onSubmit = att "data-on-submit" . toParam


submit :: Mod -> View (FormFields id) () -> View (FormFields id) ()
submit f = tag "button" (att "type" "submit" . f)


type family Field' (context :: Type -> Type) a
type instance Field' Identity a = a
type instance Field' Label a = Text
type instance Field' Invalid a = Maybe Text


parseForm :: forall form es. (Form form, Hyperbole :> es) => Eff es (form Identity)
parseForm = do
  f <- formData
  let ef = fromForm f :: Either Text (form Identity)
  either parseError pure ef


formField :: forall a es. (FormField a, Hyperbole :> es) => Eff es a
formField = do
  f <- formData
  case fieldParse (inputName @a) f of
    Left e -> parseError e
    Right a -> pure a


class Form (form :: (Type -> Type) -> Type) where
  formLabels :: form Label
  default formLabels :: (Generic (form Label), GForm (Rep (form Label))) => form Label
  formLabels = to gForm


  formInvalid :: form Invalid
  default formInvalid :: (Generic (form Invalid), GForm (Rep (form Invalid))) => form Invalid
  formInvalid = to gForm


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


instance (Selector s) => GForm (M1 S s (K1 R Text)) where
  gForm = M1 . K1 $ pack (selName (undefined :: M1 S s (K1 R Text) p))


instance GForm (M1 S s (K1 R (Maybe Text))) where
  gForm = M1 . K1 $ Nothing


class FormField a where
  inputName :: Text
  default inputName :: (Generic a, GDataName (Rep a)) => Text
  inputName = gDataName (from (undefined :: a))


  fieldParse :: Text -> FE.Form -> Either Text a
  default fieldParse :: (Generic a, GFieldParse (Rep a)) => Text -> FE.Form -> Either Text a
  fieldParse t f = to <$> gFieldParse t f


class GDataName f where
  gDataName :: f p -> Text


instance (Datatype d) => GDataName (M1 D d (M1 C c f)) where
  gDataName m1 = pack $ datatypeName m1


class GFieldParse f where
  gFieldParse :: Text -> FE.Form -> Either Text (f p)


instance (GFieldParse f) => GFieldParse (M1 D d f) where
  gFieldParse t f = M1 <$> gFieldParse t f


instance (GFieldParse f) => GFieldParse (M1 C c f) where
  gFieldParse t f = M1 <$> gFieldParse t f


instance (GFieldParse f) => GFieldParse (M1 S s f) where
  gFieldParse t f = M1 <$> gFieldParse t f


instance (FromHttpApiData a) => GFieldParse (K1 R a) where
  gFieldParse t f = K1 <$> FE.parseUnique t f
