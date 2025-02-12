{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.View.Forms
  ( Fields (..)
  , fieldNames
  , InputType (..)
  , FieldName (..)
  , Invalid
  , Input (..)
  , field
  , label
  , input
  , form
  , textarea
  , placeholder
  , submit
  , formData
  , Form (..)
  , FormParse (..)
  , formParseParam
  , formLookupParam
  , Field
  , defaultFormOptions
  , FormOptions (..)
  , Validated (..)
  , isInvalid
  , FormField (..)
  -- , anyInvalid
  , invalidText
  , validate
  , Identity

    -- * Re-exports
  , FromParam
  , Generic
  , GenFields (..)
  , GenField (..)
  )
where

import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.FormUrlEncoded (FormOptions (..), defaultFormOptions, parseUnique)
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole.Data.QueryData (FromParam (..), Param (..), ParamValue (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response (parseError)
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Event (onSubmit)
import Web.View hiding (form, input, label)
import Web.View.Style (addClass, cls, prop)


------------------------------------------------------------------------------
-- FORM
------------------------------------------------------------------------------

-- | The only time we can use Fields is inside a form
data Form id = Form id


{- | Type-safe \<form\>. Calls (Action id) on submit

@
#EMBED Example/Page/FormSimple.hs formView
@
-}
form :: (ViewAction (Action id)) => Action id -> Mod id -> View (Form id) () -> View id ()
form a md cnt = do
  vid <- context
  tag "form" (onSubmit a . md . flexCol . marginEnd0) $ do
    addContext (Form vid) cnt
 where
  -- not sure why chrome is adding margin-block-end: 16 to forms? Add to web-view?
  marginEnd0 =
    addClass $
      cls "mg-end-0"
        & prop @PxRem "margin-block-end" 0


-- | Button that submits the 'form'. Use 'button' to specify actions other than submit
submit :: Mod (Form id) -> View (Form id) () -> View (Form id) ()
submit f = tag "button" (att "type" "submit" . f)


formData :: forall form es. (FormParse form, Hyperbole :> es) => Eff es form
formData = do
  f <- formBody
  let ef = formParse @form f :: Either Text form
  either parseError pure ef


formParseParam :: (FromParam a) => Param -> FE.Form -> Either Text a
formParseParam (Param key) frm = do
  t <- FE.parseUnique @Text key frm
  parseParam (ParamValue t)


formLookupParam :: (FromParam a) => Param -> FE.Form -> Either Text (Maybe a)
formLookupParam (Param key) frm = do
  mt <- FE.parseMaybe @Text key frm
  maybe (pure Nothing) (parseParam . ParamValue) mt


{- | A Form is a Higher Kinded record listing each 'Field'. `ContactForm` `Identity` behaves like a normal record, while `ContactForm` `Maybe` would be maybe values for each field

From [Example.Page.FormSimple](https://docs.hyperbole.live/formsimple)

@
#EMBED Example/Page/FormSimple.hs data ContactForm
#EMBED Example/Page/FormSimple.hs instance Form ContactForm
@
-}

-- so if you use an HKD, you would do instance FormParse MyForm
class FormParse form where
  formParse :: FE.Form -> Either Text form
  default formParse :: (Generic form, GFormParse (Rep form)) => FE.Form -> Either Text form
  formParse f = to <$> gFormParse f


------------------------------------------------------------------------------
-- FORM FIELDS
------------------------------------------------------------------------------

newtype FieldName a = FieldName Text
  deriving newtype (Show, IsString)


data FormField f a = FormField
  { name :: FieldName a
  , value :: f a
  }
  deriving (Show)


-- | Display a 'FormField'. See 'form' and 'Form'
field
  :: forall (id :: Type) (f :: Type -> Type) (a :: Type)
   . (ToFieldName f)
  => f a
  -> Mod (Form id)
  -> View (Input id a) ()
  -> View (Form id) ()
field fa f inputs = do
  tag "label" (f . flexCol) $ do
    addContext (Input $ fieldName fa) inputs


class ToFieldName f where
  fieldName :: f a -> FieldName a


instance ToFieldName FieldName where
  fieldName = id


instance ToFieldName (FormField f) where
  fieldName (FormField n _) = n


class Fields (form :: (Type -> Type) -> Type) f where
  -- {- | Generate a Higher Kinded Type (form f)
  --
  -- > #EMBED Example/Page/FormSimple.hs data ContactForm'
  -- >
  -- > #EMBED Example/Page/FormSimple.hs formView'
  -- -}
  genFields :: form f
  default genFields :: (Generic (form f), GenFields (Rep (form f))) => form f
  genFields = to gGenFields


  -- | Generate FormFields for the given instance of 'Form' from validation data. See [Example.Page.FormValidation](https://docs.hyperbole.live/formvalidation)
  --
  --   > #EMBED Example/Page/FormValidation.hs data UserForm
  --   > #EMBED Example/Page/FormValidation.hs instance Form UserForm
  --   >
  --   > #EMBED Example/Page/FormValidation.hs formView
  formFields :: form f -> form (FormField f)
  default formFields
    :: (Generic (form f), Generic (form (FormField f)), GConvert (Rep (form f)) (Rep (form (FormField f))))
    => form f
    -> form (FormField f)
  formFields fv = to $ gConvert (from fv)


-- {- | Generate Form Fieldnames. See [Example.Page.FormSimple](https://docs.hyperbole.live/formsimple)
--
-- > #EMBED Example/Page/FormSimple.hs data ContactForm'
-- >
-- > #EMBED Example/Page/FormSimple.hs formView'
-- -}
fieldNames :: forall form. (Fields form FieldName) => form FieldName
fieldNames = genFields


------------------------------------------------------------------------------
-- INPUTS
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------
-- VALIDATION
------------------------------------------------------------------------------

{- | Validation results for a 'Form'. See 'validate'

@
#EMBED Example/Page/FormValidation.hs data UserForm
#EMBED Example/Page/FormValidation.hs instance Form UserForm

#EMBED Example/Page/FormValidation.hs validateForm

#EMBED Example/Page/FormValidation.hs validateAge
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


instance (FromParam a, ValidateField a) => FromParam (Validated a) where
  parseParam inp = do
    a <- parseParam @a inp
    pure $ validateField a


isInvalid :: Validated a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False


class ValidateField a where
  validateField :: a -> Validated a


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
#EMBED Example/Page/FormValidation.hs validateAge
@
-}
validate :: Bool -> Text -> Validated a
validate True t = Invalid t -- Validation [(inputName @a, Invalid t)]
validate False _ = NotInvalid -- Validation [(inputName @a, NotInvalid)]


-- validateWith :: forall a fs v. (FormField a, Elem a fs, ValidationState v) => v a -> Validation' v fs
-- validateWith v = Validation [(inputName @a, convert v)]

-- anyInvalid :: forall form val. (Form form val, ValidationState val) => form val -> Bool
-- anyInvalid f = any isInvalid (collectValids f :: [val ()])

-- any (isInvalid . snd) vs

{- | Returns the 'Validated' for the 'field'. See 'formFieldsWith'
fieldValid :: View (Input id (Validated a)) (Validated a)
fieldValid = do
  Input _ v <- context
  pure v
-}
data Invalid a


{- | Field allows a Higher Kinded 'Form' to reuse the same selectors for form parsing, generating html forms, and validation

> Field Identity Text ~ Text
> Field Maybe Text ~ Maybe Text
-}
type family Field (context :: Type -> Type) a


type instance Field (FormField f) a = FormField f a
type instance Field Identity a = a
type instance Field FieldName a = FieldName a
type instance Field Validated a = Validated a
type instance Field Maybe a = Maybe a
type instance Field (Either String) a = Either String a


------------------------------------------------------------------------------
-- GEN FORM PARSE
------------------------------------------------------------------------------

-- | Automatically derive labels from form field names
class GFormParse f where
  gFormParse :: FE.Form -> Either Text (f p)


instance (GFormParse f, GFormParse g) => GFormParse (f :*: g) where
  gFormParse f = do
    a <- gFormParse f
    b <- gFormParse f
    pure $ a :*: b


instance (GFormParse f) => GFormParse (M1 D d f) where
  gFormParse f = M1 <$> gFormParse f


instance (GFormParse f) => GFormParse (M1 C c f) where
  gFormParse f = M1 <$> gFormParse f


instance (Selector s, FromParam a) => GFormParse (M1 S s (K1 R a)) where
  gFormParse f = do
    let s = selName (undefined :: M1 S s (K1 R (f a)) p)
    t <- parseUnique @Text (pack s) f
    M1 . K1 <$> parseParam (ParamValue t)


------------------------------------------------------------------------------
-- GEN FIELDS :: Create the field! -------------------------------------------
------------------------------------------------------------------------------

class GenFields f where
  gGenFields :: f p


instance GenFields U1 where
  gGenFields = U1


instance (GenFields f, GenFields g) => GenFields (f :*: g) where
  gGenFields = gGenFields :*: gGenFields


-- instance (Selector s, GenField g a, Field f a ~ g a) => GenFields (M1 S s (K1 R (g a))) where
--   gGenFields =
--     let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
--      in M1 . K1 $ genField @g @a sel

instance (Selector s, GenField a) => GenFields (M1 S s (K1 R a)) where
  gGenFields =
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 $ genField @a sel


-- -  genForm :: form val
-- -  default genForm :: (Generic (form val), GenFields (Rep (form val))) => form val
-- -  genForm = to gGenFields

instance (GenFields f) => GenFields (M1 D d f) where
  gGenFields = M1 gGenFields


instance (GenFields f) => GenFields (M1 C c f) where
  gGenFields = M1 gGenFields


------------------------------------------------------------------------------
-- GenField -- Generate a value from the selector name
------------------------------------------------------------------------------

class GenField a where
  genField :: String -> a


instance GenField (FieldName a) where
  genField s = FieldName $ pack s


instance GenField (Validated a) where
  genField = const NotInvalid


instance (GenField (f a)) => GenField (FormField f a) where
  genField s = FormField (FieldName $ pack s) (genField s)


instance GenField (Maybe a) where
  genField _ = Nothing


------------------------------------------------------------------------------
-- GMerge - combine two records with the same structure
------------------------------------------------------------------------------

class GMerge ra rb rc where
  gMerge :: ra p -> rb p -> rc p


instance (GMerge ra0 rb0 rc0, GMerge ra1 rb1 rc1) => GMerge (ra0 :*: ra1) (rb0 :*: rb1) (rc0 :*: rc1) where
  gMerge (a0 :*: a1) (b0 :*: b1) = gMerge a0 b0 :*: gMerge a1 b1


instance (GMerge ra rb rc) => GMerge (M1 D d ra) (M1 D d rb) (M1 D d rc) where
  gMerge (M1 fa) (M1 fb) = M1 $ gMerge fa fb


instance (GMerge ra rb rc) => GMerge (M1 C d ra) (M1 C d rb) (M1 C d rc) where
  gMerge (M1 fa) (M1 fb) = M1 $ gMerge fa fb


instance (Selector s, MergeField a b c) => GMerge (M1 S s (K1 R a)) (M1 S s (K1 R b)) (M1 S s (K1 R c)) where
  gMerge (M1 (K1 a)) (M1 (K1 b)) = M1 . K1 $ mergeField a b


class MergeField a b c where
  mergeField :: a -> b -> c


-- instance MergeField (FieldName a) (Validated a) (FormField Validated a) where
--   mergeField = FormField

------------------------------------------------------------------------------
-- GConvert - combine two records with the same structure
------------------------------------------------------------------------------

-- class ConvertFields a where
--   convertFields :: (FromSelector f g) => a f -> a g
--   default convertFields :: (Generic (a f), Generic (a g), GConvert (Rep (a f)) (Rep (a g))) => a f -> a g
--   convertFields x = to $ gConvert (from x)

class GConvert ra rc where
  gConvert :: ra p -> rc p


instance (GConvert ra0 rc0, GConvert ra1 rc1) => GConvert (ra0 :*: ra1) (rc0 :*: rc1) where
  gConvert (a0 :*: a1) = gConvert a0 :*: gConvert a1


instance (GConvert ra rc) => GConvert (M1 D d ra) (M1 D d rc) where
  gConvert (M1 fa) = M1 $ gConvert fa


instance (GConvert ra rc) => GConvert (M1 C d ra) (M1 C d rc) where
  gConvert (M1 fa) = M1 $ gConvert fa


instance (Selector s, GenFieldFrom f g a, Field g a ~ g a) => GConvert (M1 S s (K1 R (f a))) (M1 S s (K1 R (g a))) where
  gConvert (M1 (K1 inp)) =
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 $ genFieldFrom @f @g sel inp


class GenFieldFrom inp f a where
  genFieldFrom :: String -> inp a -> Field f a


-- instance GenFieldFrom Validated (FormField Validated) a where
--   genFieldFrom s = FormField (FieldName $ pack s)

instance GenFieldFrom val (FormField val) a where
  genFieldFrom s = FormField (FieldName $ pack s)

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

------------------------------------------------------------------------------

-- -- this is ANY f, not just the one we want
-- data ContactForm f = ExampleForm
--   { name :: Field f Text
--   , age :: Field f Int
--   }
--   deriving (Generic)
-- instance FormParse (ContactForm Identity)
-- instance Fields ContactForm Maybe
-- instance Fields ContactForm FieldName
-- instance Fields ContactForm Validated
--
--
-- -- instance FormParse (ContactForm Validated)
--
-- formView :: (ViewAction (Action id)) => View id ()
-- formView = do
--   -- generate a ContactForm' FieldName
--   let f = fieldNames @ContactForm
--   form undefined (gap 10 . pad 10) $ do
--     -- f.name :: FieldName Text
--     -- f.name = FieldName "name"
--     field f.name id $ do
--       label "Contact Name"
--       input Username (placeholder "contact name")
--
--     -- f.age :: FieldName Int
--     -- f.age = FieldName "age"
--     field f.age id $ do
--       label "Age"
--       input Number (placeholder "age" . value "0")
--
--     submit id "Submit"
--
--
-- formView' :: (ViewAction (Action id)) => ContactForm Validated -> View id ()
-- formView' contact = do
--   -- generate a ContactForm' FieldName
--   let f = formFields @ContactForm contact
--   form undefined (gap 10 . pad 10) $ do
--     -- f.name :: FieldName Text
--     -- f.name = FieldName "name"
--     field f.name id $ do
--       label "Contact Name"
--       input Username (placeholder "contact name")
--
--     -- f.age :: FieldName Int
--     -- f.age = FieldName "age"
--     field f.age id $ do
--       label "Age"
--       input Number (placeholder "age" . value "0")
--
--     field f.age id $ do
--       label "Username"
--       input Username (placeholder "username")
--
--     case f.age.value of
--       Invalid t -> el_ (text t)
--       Valid -> el_ "Username is available"
--       _ -> none
--
--     submit id "Submit"
--  where
--   valStyle (Invalid _) = id
--   valStyle Valid = id
--   valStyle _ = id
--
--
-- data ContactForm' = ContactForm'
--   { name :: Text
--   , age :: Int
--   }
--   deriving (Generic)
-- instance FormParse ContactForm'
--
--
-- formView'' :: (ViewAction (Action id)) => View id ()
-- formView'' = do
--   form undefined (gap 10 . pad 10) $ do
--     -- f.name :: FieldName Text
--     -- f.name = FieldName "name"
--     field (FieldName "name") id $ do
--       label "Contact Name"
--       input Username (placeholder "contact name")
