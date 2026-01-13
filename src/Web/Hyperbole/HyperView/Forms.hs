{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Forms
  ( FromForm (..)
  , FromFormF (..)
  , GenFields (..)
  , fieldNames
  , FieldName (..)
  , FormFields (..)
  , Field
  , InputType (..)
  , Input (..)
  , field
  , label
  , input
  , checkbox
  , radioGroup
  , radio
  , select
  , form
  , textarea
  , submit
  , formData
  , FormOptions (..)
  , Validated (..)
  , isInvalid
  , invalidText
  , validate
  , Identity

    -- * Re-exports
  , FE.FromFormKey
  , Generic
  , GFieldsGen (..)
  , GenField (..)
  , Form (..)
  )
where

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.Atomic.Types hiding (Selector)
import Web.FormUrlEncoded (Form (..), FormOptions (..))
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole.Data.Param
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response (parseError)
import Web.Hyperbole.HyperView.Event (onSubmit)
import Web.Hyperbole.HyperView.Input (Option (..), checked)
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.View


------------------------------------------------------------------------------
-- FORM PARSING
------------------------------------------------------------------------------

{- | Simple types that be decoded from form data

@
#EMBED Example.FormSimple data ContactForm
@
-}
class FromForm (form :: Type) where
  fromForm :: FE.Form -> Either String form
  default fromForm :: (Generic form, GFormParse (Rep form)) => FE.Form -> Either String form
  fromForm f = to <$> gFormParse f


{- | A Higher-Kinded type that can be parsed from a 'Web.FormUrlEncoded.Form'

@
#EMBED Example.FormValidation data UserForm
@
-}
class FromFormF (f :: (Type -> Type) -> Type) where
  fromFormF :: FE.Form -> Either String (f Identity)
  default fromFormF :: (Generic (f Identity), GFormParse (Rep (f Identity))) => FE.Form -> Either String (f Identity)
  fromFormF f = to <$> gFormParse f


-- Any FromFormF can be parsed using fromForm @(form Identity)
-- we can't make it an instance because it is an orphan instance
instance (FromFormF form) => FromForm (form Identity) where
  fromForm = fromFormF


-- | Parse a full type from a submitted form body
formData :: forall form es. (FromForm form, Hyperbole :> es) => Eff es form
formData = do
  f <- formBody
  let ef = fromForm @form f :: Either String form
  either parseError pure ef


------------------------------------------------------------------------------
-- GEN FIELDS: Generate a type from selector names
------------------------------------------------------------------------------

{- | Generate a Higher Kinded record with all selectors filled with default values. See 'GenField'

@
#EMBED Example.FormValidation data UserForm
@

@
#EMBED Example.Contacts newContactForm
@
-}
class GenFields f (form :: (Type -> Type) -> Type) where
  genFields :: form f
  default genFields :: (Generic (form f), GFieldsGen (Rep (form f))) => form f
  genFields = to gFieldsGen


{- | Generate FieldNames for a form

#EXAMPLE /forms

> #EMBED Example.Todos.Todo data TodoForm
>
> #EMBED Example.Todos.Todo todoForm
-}
fieldNames :: forall form. (GenFields FieldName form) => form FieldName
fieldNames = genFields


-- Given a selector, generate the type
class GenField a where
  genField :: String -> a


instance GenField (FieldName a) where
  genField s = FieldName $ pack s


instance GenField (Validated a) where
  genField = const NotInvalid


instance GenField (Maybe a) where
  genField _ = Nothing


------------------------------------------------------------------------------
-- FORM VIEWS
------------------------------------------------------------------------------

-- | Context that allows form fields
newtype FormFields id = FormFields id
  deriving (Generic)
  deriving newtype (ViewId)


{- | Type-safe \<form\>. Calls (Action id) on submit

@
#EMBED Example.FormSimple formView
@
-}
form :: (ViewAction (Action id)) => Action id -> View (FormFields id) () -> View id ()
form a cnt = do
  tag "form" @ onSubmit a $ do
    runChildView FormFields cnt


-- | Button that submits the 'form'
submit :: View (FormFields id) () -> View (FormFields id) ()
submit = tag "button" @ att "type" "submit"


-- | Form FieldName. This is embeded as the name attribute, and refers to the key need to parse the form when submitted. See 'fieldNames'
newtype FieldName a = FieldName {value :: Text}
  deriving newtype (Show, IsString, FromParam, ToParam)


-- | Display a 'FormField'. See 'form' and 'Form'
field
  :: forall (id :: Type) (a :: Type)
   . FieldName a
  -> View (Input id a) ()
  -> View (FormFields id) ()
field fn =
  runChildView (\(FormFields i) -> Input i fn)


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
  { id :: id
  , inputName :: FieldName a
  }
  deriving (Generic)
instance (ViewId id, FromParam id, ToParam id) => ViewId (Input id a) where
  type ViewState (Input id a) = ViewState id


{- | label for a 'field'
label :: Text -> View (Input id a) ()
-}
label :: View c () -> View c ()
label = tag "label"


-- | input for a 'field'
input :: forall id a. InputType -> View (Input id a) ()
input ft = do
  inp :: Input id a <- viewId
  tag "input" @ att "type" (inpType ft) . name inp.inputName.value . att "autocomplete" (auto ft) $ none
 where
  inpType NewPassword = "password"
  inpType CurrentPassword = "password"
  inpType Number = "number"
  inpType Email = "email"
  inpType Search = "search"
  inpType _ = "text"

  auto :: InputType -> Text
  auto TextInput = "off"
  auto inp = pack . kebab . show $ inp


checkbox :: forall id a. Bool -> View (Input id a) ()
checkbox isChecked = do
  inp :: Input id a <- viewId
  tag "input" @ att "type" "checkbox" . name inp.inputName.value $ none @ checked isChecked


-- NOTE: Radio is a special type of selection different from list type or
-- select. select or list input can be thought of one wrapper and multiple
-- options whereas radio is multiple wrappers with options. The context required
-- for radio is more than that required for select.
data Radio (id :: Type) (a :: Type) (opt :: Type) = Radio
  { id :: id
  , inputName :: FieldName a
  , defaultOption :: opt
  }
  deriving (Generic)
instance (FromParam id, ToParam id, FromParam a, ToParam a, ToParam opt, FromParam opt) => ViewId (Radio id a opt) where
  type ViewState (Radio id a opt) = ViewState id


radioGroup :: opt -> View (Radio id a opt) () -> View (Input id a) ()
radioGroup defOpt = runChildView (\(inp :: Input id a) -> Radio inp.id inp.inputName defOpt)


radio :: forall id a opt. (Eq opt, ToParam opt) => opt -> View (Radio id a opt) ()
radio val = do
  rd :: Radio id a opt <- viewId
  tag "input"
    @ att "type" "radio"
    . name rd.inputName.value
    . value (toParam val).value
    . checked (rd.defaultOption == val)
    $ none


select :: forall opt id a. (Eq opt) => opt -> View (Option id opt) () -> View (Input id a) ()
select defOpt options = do
  inp :: Input id a <- viewId
  tag "select" @ name inp.inputName.value $ runChildView (\_ -> Option inp.id defOpt) options


-- | textarea for a 'field'
textarea :: forall id a. Maybe Text -> View (Input id a) ()
textarea mDefaultText = do
  inp :: Input id a <- viewId
  tag "textarea" @ name inp.inputName.value $ text $ fromMaybe "" mDefaultText


------------------------------------------------------------------------------
-- VALIDATION
------------------------------------------------------------------------------

{- | Validation results for a 'Form'. See 'validate'

@
#EMBED Example.FormValidation data UserForm

#EMBED Example.FormValidation validateForm

#EMBED Example.FormValidation validateAge
@
-}
data Validated a = Invalid Text | NotInvalid | Valid
  deriving (Show)


instance Semigroup (Validated a) where
  Invalid t <> _ = Invalid t
  _ <> Invalid t = Invalid t
  Valid <> _ = Valid
  _ <> Valid = Valid
  a <> _ = a


instance Monoid (Validated a) where
  mempty = NotInvalid


-- instance (FromParam a, ValidateField a) => FromParam (Validated a) where
--   parseParam inp = do
--     a <- parseParam @a inp
--     pure $ validateField a

isInvalid :: Validated a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False


-- class ValidateField a where
--   validateField :: a -> Validated a
--

-- class ValidationState (v :: Type -> Type) where
--   convert :: v a -> v b
--   isInvalid :: v a -> Bool
--
--
-- instance ValidationState Validated where
--   convert :: Validated a -> Validated b
--   convert (Invalid t) = Invalid t
--   convert NotInvalid = NotInvalid
--   convert Valid = Valid
--
--

{- Only shows if 'Validated' is 'Invalid'. See 'formFieldsWith'formform
@
@
-}
invalidText :: forall a id. Validated a -> View (Input id a) ()
invalidText v = do
  case v of
    Invalid t -> text t
    _ -> none


{- | specify a check for a 'Validation'

@
#EMBED Example.FormValidation validateAge
@
-}
validate :: Bool -> Text -> Validated a
validate True t = Invalid t -- Validation [(inputName @a, Invalid t)]
validate False _ = NotInvalid -- Validation [(inputName @a, NotInvalid)]


{- | Field allows a Higher Kinded 'Form' to reuse the same selectors for form parsing, generating html forms, and validation

> Field Identity Text ~ Text
> Field Maybe Text ~ Maybe Text
-}
type family Field (context :: Type -> Type) a


-- type instance Field (FormField f) a = FormField f a
type instance Field Identity a = a
type instance Field FieldName a = FieldName a
type instance Field Validated a = Validated a
type instance Field Maybe a = Maybe a
type instance Field (Either String) a = Either String a


------------------------------------------------------------------------------
-- GENERIC FORM PARSE
------------------------------------------------------------------------------

class GFormParse f where
  gFormParse :: FE.Form -> Either String (f p)


instance (GFormParse f, GFormParse g) => GFormParse (f :*: g) where
  gFormParse f = do
    a <- gFormParse f
    b <- gFormParse f
    pure $ a :*: b


instance (GFormParse f) => GFormParse (M1 D d f) where
  gFormParse f = M1 <$> gFormParse f


instance (GFormParse f) => GFormParse (M1 C c f) where
  gFormParse f = M1 <$> gFormParse f


-- TODO: need a bool instance?
-- TODO: need a Maybe a instance?
instance (Selector s, FromParam a) => GFormParse (M1 S s (K1 R a)) where
  -- these CANNOT be json encoded, they are encoded by the browser
  gFormParse f = do
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
    mt :: Maybe Text <- first cs $ FE.lookupMaybe (cs sel) f
    a <- first (\err -> sel <> ": " <> err) $ decodeFormValue mt
    pure $ M1 . K1 $ a


-- instance {-# OVERLAPPING #-} (Selector s, FromParam a) => GFormParse (M1 S s (K1 R (Maybe a))) where
--   gFormParse f = do
--     let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
--     mt :: Maybe Text <- first cs $ FE.lookupMaybe (cs sel) f
--     ma :: Maybe a <- maybe (pure Nothing) (parseParam . decodeParam) mt
--     pure $ M1 . K1 $ ma

------------------------------------------------------------------------------
-- GENERIC GENERATE FIELDS
------------------------------------------------------------------------------

class GFieldsGen f where
  gFieldsGen :: f p


instance GFieldsGen U1 where
  gFieldsGen = U1


instance (GFieldsGen f, GFieldsGen g) => GFieldsGen (f :*: g) where
  gFieldsGen = gFieldsGen :*: gFieldsGen


-- instance (Selector s, GenField g a, Field f a ~ g a) => GFieldsGen (M1 S s (K1 R (g a))) where
--   gFieldsGen =
--     let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
--      in M1 . K1 $ genField @g @a sel

instance (Selector s, GenField a) => GFieldsGen (M1 S s (K1 R a)) where
  gFieldsGen =
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 $ genField @a sel


instance (GFieldsGen f) => GFieldsGen (M1 D d f) where
  gFieldsGen = M1 gFieldsGen


instance (GFieldsGen f) => GFieldsGen (M1 C c f) where
  gFieldsGen = M1 gFieldsGen

------------------------------------------------------------------------------
-- GMerge - combine two records with the same structure
------------------------------------------------------------------------------

-- class GMerge ra rb rc where
--   gMerge :: ra p -> rb p -> rc p
--
--
-- instance (GMerge ra0 rb0 rc0, GMerge ra1 rb1 rc1) => GMerge (ra0 :*: ra1) (rb0 :*: rb1) (rc0 :*: rc1) where
--   gMerge (a0 :*: a1) (b0 :*: b1) = gMerge a0 b0 :*: gMerge a1 b1
--
--
-- instance (GMerge ra rb rc) => GMerge (M1 D d ra) (M1 D d rb) (M1 D d rc) where
--   gMerge (M1 fa) (M1 fb) = M1 $ gMerge fa fb
--
--
-- instance (GMerge ra rb rc) => GMerge (M1 C d ra) (M1 C d rb) (M1 C d rc) where
--   gMerge (M1 fa) (M1 fb) = M1 $ gMerge fa fb
--
--
-- instance (Selector s, MergeField a b c) => GMerge (M1 S s (K1 R a)) (M1 S s (K1 R b)) (M1 S s (K1 R c)) where
--   gMerge (M1 (K1 a)) (M1 (K1 b)) = M1 . K1 $ mergeField a b
--
--
-- class MergeField a b c where
--   mergeField :: a -> b -> c

-- instance MergeField (FieldName a) (Validated a) (FormField Validated a) where
--   mergeField = FormField

------------------------------------------------------------------------------
-- GConvert - combine two records with the same structure
------------------------------------------------------------------------------

-- class ConvertFields a where
--   convertFields :: (FromSelector f g) => a f -> a g
--   default convertFields :: (Generic (a f), Generic (a g), GConvert (Rep (a f)) (Rep (a g))) => a f -> a g
--   convertFields x = to $ gConvert (from x)
--
-- class GConvert ra rc where
--   gConvert :: ra p -> rc p
--
--
-- instance (GConvert ra0 rc0, GConvert ra1 rc1) => GConvert (ra0 :*: ra1) (rc0 :*: rc1) where
--   gConvert (a0 :*: a1) = gConvert a0 :*: gConvert a1
--
--
-- instance (GConvert ra rc) => GConvert (M1 D d ra) (M1 D d rc) where
--   gConvert (M1 fa) = M1 $ gConvert fa
--
--
-- instance (GConvert ra rc) => GConvert (M1 C d ra) (M1 C d rc) where
--   gConvert (M1 fa) = M1 $ gConvert fa
--
--
-- instance (Selector s, GenFieldFrom f g a, Field g a ~ g a) => GConvert (M1 S s (K1 R (f a))) (M1 S s (K1 R (g a))) where
--   gConvert (M1 (K1 inp)) =
--     let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
--      in M1 . K1 $ genFieldFrom @f @g sel inp
--
--
-- class GenFieldFrom inp f a where
--   genFieldFrom :: String -> inp a -> Field f a
--
--
-- -- instance GenFieldFrom Validated (FormField Validated) a where
-- --   genFieldFrom s = FormField (FieldName $ pack s)
--
-- instance GenFieldFrom val (FormField val) a where
--   genFieldFrom s = FormField (FieldName $ pack s)

------------------------------------------------------------------------------

-- class GCollect ra v where
--   gCollect :: ra p -> [v ()]
--
--
-- instance GCollect U1 v where
--   gCollect _ = []
--
--
-- instance (GCollect f v, GCollect g v) => GCollect (f :*: g) v where
--   gCollect (a :*: b) = gCollect a <> gCollect b
--
--
-- instance (Selector s, ValidationState v) => GCollect (M1 S s (K1 R (v a))) v where
--   gCollect (M1 (K1 val)) = [convert val]
--
--
-- instance (GCollect f v) => GCollect (M1 D d f) v where
--   gCollect (M1 f) = gCollect f
--
--
-- instance (GCollect f v) => GCollect (M1 C c f) v where
--   gCollect (M1 f) = gCollect f
