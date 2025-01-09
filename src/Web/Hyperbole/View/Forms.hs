{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Web.Hyperbole.View.Forms
  ( FormFields (..)
  , InputType (..)
  , FieldName
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
  , formParseParam
  , formLookupParam
  , formFields
  , formFieldsWith
  , Field
  , defaultFormOptions
  , FormOptions (..)
  , Validated (..)
  , FormField (..)
  , fieldValid
  , anyInvalid
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
import Data.Text (Text, pack)
import Debug.Trace
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


-- | The only time we can use Fields is inside a form
data FormFields id = FormFields id


data FormField v a = FormField
  { fieldName :: FieldName a
  , validated :: v a
  }
  deriving (Show)


-- instance Show (v a) => Show (FormField v) where
--   show f = "Form Field"

-- instance (ViewId id) => ViewId (FormFields id v fs) where
--   parseViewId t = do
--     i <- parseViewId t
--     pure $ FormFields i lbls mempty
--   toViewId (FormFields i _ _) = toViewId i
--
--
-- instance (HyperView id, ViewId id) => HyperView (FormFields id v fs) where
--   type Action (FormFields id v fs) = Action id

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


class ValidationState (v :: Type -> Type) where
  convert :: v a -> v b
  isInvalid :: v a -> Bool


instance ValidationState Validated where
  convert :: Validated a -> Validated b
  convert (Invalid t) = Invalid t
  convert NotInvalid = NotInvalid
  convert Valid = Valid


  isInvalid :: Validated a -> Bool
  isInvalid (Invalid _) = True
  isInvalid _ = False


{- Only shows if 'Validated' is 'Invalid'. See 'formFieldsWith'
@
@
-}
invalidText :: forall a id. View (Input id Validated a) ()
invalidText = do
  Input _ v <- context
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

-- eh... not sure how to do this...
anyInvalid :: forall form val. (Form form val, ValidationState val) => form val -> Bool
anyInvalid f = any isInvalid (collectValids f :: [val ()])


-- any (isInvalid . snd) vs

-- | Returns the 'Validated' for the 'field'. See 'formFieldsWith'
fieldValid :: View (Input id v a) (v a)
fieldValid = do
  Input _ v <- context
  pure v


data FieldName a = FieldName Text
  deriving (Show)


data Invalid a


data Input (id :: Type) (valid :: Type -> Type) (a :: Type) = Input
  { inputName :: FieldName a
  , valid :: valid a
  }


-- | Display a 'FormField'. See 'form' and 'Form'
field
  :: forall (id :: Type) (v :: Type -> Type) (a :: Type)
   . FormField v a
  -> (v a -> Mod (FormFields id))
  -> View (Input id v a) ()
  -> View (FormFields id) ()
field fld md cnt = do
  tag "label" (md fld.validated . flexCol) $ do
    addContext (Input fld.fieldName fld.validated) cnt


-- | label for a 'field'
label :: Text -> View (Input id v a) ()
label = text


-- | input for a 'field'
input :: InputType -> Mod (Input id v a) -> View (Input id v a) ()
input ft f = do
  Input (FieldName nm) _ <- context
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
textarea :: Mod (Input id v a) -> Maybe Text -> View (Input id v a) ()
textarea f mDefaultText = do
  Input (FieldName nm) _ <- context
  tag "textarea" (f . name nm) (text $ fromMaybe "" mDefaultText)


{- | Type-safe \<form\>. Calls (Action id) on submit

@
#EMBED Example/Page/FormSimple.hs formView
@
-}
form :: (Form form v, ViewAction (Action id)) => Action id -> Mod id -> View (FormFields id) () -> View id ()
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


-- | Button that submits the 'form'. Use 'button' to specify actions other than submit
submit :: Mod (FormFields id) -> View (FormFields id) () -> View (FormFields id) ()
submit f = tag "button" (att "type" "submit" . f)


{- | Field allows a Higher Kinded 'Form' to reuse the same selectors for form parsing, generating html forms, and validation

> Field Identity Text ~ Text
> Field Maybe Text ~ Maybe Text
-}
type family Field (context :: Type -> Type) a


type instance Field Identity a = a
type instance Field FieldName a = FieldName a
type instance Field (FormField v) a = FormField v a
type instance Field Validated a = Validated a
type instance Field Maybe a = Maybe a
type instance Field (Either String) a = Either String a


formData :: forall form val es. (Form form val, Hyperbole :> es) => Eff es (form Identity)
formData = do
  f <- formBody
  traceM $ show f
  let ef = formParse @form @val f :: Either Text (form Identity)
  case ef of
    Left e -> parseError e
    Right a -> pure a


{- | A Form is a Higher Kinded record listing each 'Field'. `ContactForm` `Identity` behaves like a normal record, while `ContactForm` `Maybe` would be maybe values for each field

From [Example.Page.FormSimple](https://docs.hyperbole.live/formsimple)

@
#EMBED Example/Page/FormSimple.hs data ContactForm
#EMBED Example/Page/FormSimple.hs instance Form ContactForm
@
-}
class Form form (val :: Type -> Type) | form -> val where
  formParse :: FE.Form -> Either Text (form Identity)
  default formParse :: (Generic (form Identity), GFormParse (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
  formParse f = to <$> gFormParse f


  collectValids :: (ValidationState val) => form val -> [val ()]
  default collectValids :: (Generic (form val), GCollect (Rep (form val)) val) => form val -> [val ()]
  collectValids f = gCollect (from f)


  genForm :: form val
  default genForm :: (Generic (form val), GenFields (Rep (form val))) => form val
  genForm = to gGenFields


  genFieldsWith :: form val -> form (FormField val)
  default genFieldsWith
    :: (Generic (form val), Generic (form (FormField val)), GConvert (Rep (form val)) (Rep (form (FormField val))))
    => form val
    -> form (FormField val)
  genFieldsWith fv = to $ gConvert (from fv)


formParseParam :: (FromParam a) => Param -> FE.Form -> Either Text a
formParseParam (Param key) frm = do
  t <- FE.parseUnique @Text key frm
  parseParam (ParamValue t)


formLookupParam :: (FromParam a) => Param -> FE.Form -> Either Text (Maybe a)
formLookupParam (Param key) frm = do
  mt <- FE.parseMaybe @Text key frm
  maybe (pure Nothing) (parseParam . ParamValue) mt


{- | Generate FormFields for the given instance of 'Form', with no validation information. See [Example.Page.FormSimple](https://docs.hyperbole.live/formsimple)

> #EMBED Example/Page/FormSimple.hs data ContactForm
>
> #EMBED Example/Page/FormSimple.hs formView
-}
formFields :: (Form form val) => form (FormField val)
formFields = genFieldsWith genForm


{- | Generate FormFields for the given instance of 'Form' from validation data. See [Example.Page.FormValidation](https://docs.hyperbole.live/formvalidation)

> #EMBED Example/Page/FormValidation.hs data UserForm
> #EMBED Example/Page/FormValidation.hs instance Form UserForm
>
> #EMBED Example/Page/FormValidation.hs formView
-}
formFieldsWith :: (Form form val) => form val -> form (FormField val)
formFieldsWith = genFieldsWith


-- | Automatically derive labels from form field names
class GFormParse f where
  gFormParse :: FE.Form -> Either Text (f p)


-- instance GForm U1 where
--   gForm = U1

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


instance (Selector s, GenField f a, Field f a ~ f a) => GenFields (M1 S s (K1 R (f a))) where
  gGenFields =
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 $ genField @f @a sel


instance (GenFields f) => GenFields (M1 D d f) where
  gGenFields = M1 gGenFields


instance (GenFields f) => GenFields (M1 C c f) where
  gGenFields = M1 gGenFields


------------------------------------------------------------------------------
-- GenField -- Generate a value from the selector name
------------------------------------------------------------------------------

class GenField f a where
  genField :: String -> Field f a


instance GenField FieldName a where
  genField s = FieldName $ pack s


instance GenField Validated a where
  genField = const NotInvalid


instance GenField (FormField Validated) a where
  genField s = FormField (FieldName $ pack s) NotInvalid


instance GenField (FormField Maybe) a where
  genField s = FormField (FieldName $ pack s) Nothing


instance GenField Maybe a where
  genField _ = Nothing


------------------------------------------------------------------------------
-- GMerge - combine two records with the same structure
------------------------------------------------------------------------------

-- class ConvertFields a where
--   convertFields :: (FromSelector f g) => a f -> a g
--   default convertFields :: (Generic (a f), Generic (a g), GConvert (Rep (a f)) (Rep (a g))) => a f -> a g
--   convertFields x = to $ gConvert (from x)

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


instance MergeField (FieldName a) (Validated a) (FormField Validated a) where
  mergeField = FormField


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
--

instance GenFieldFrom val (FormField val) a where
  genFieldFrom s = FormField (FieldName $ pack s)


------------------------------------------------------------------------------

class GCollect ra v where
  gCollect :: ra p -> [v ()]


instance GCollect U1 v where
  gCollect _ = []


instance (GCollect f v, GCollect g v) => GCollect (f :*: g) v where
  gCollect (a :*: b) = gCollect a <> gCollect b


instance (Selector s, ValidationState v) => GCollect (M1 S s (K1 R (v a))) v where
  gCollect (M1 (K1 val)) = [convert val]


instance (GCollect f v) => GCollect (M1 D d f) v where
  gCollect (M1 f) = gCollect f


instance (GCollect f v) => GCollect (M1 C c f) v where
  gCollect (M1 f) = gCollect f

------------------------------------------------------------------------------
