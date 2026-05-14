{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Forms
  ( FromForm (..)
  , FromFormF (..)
  , FromField (..)
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
  , fileInput
  , checkbox
  , radioGroup
  , radio
  , select
  , form
  , textarea
  , submit
  , formData
  , Validated (..)
  , isInvalid
  , invalidText
  , validate
  , Identity

    -- * Re-exports
  , Generic
  , GFieldsGen (..)
  , GenField (..)
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import Data.ByteString qualified as BS
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Effectful
import GHC.Generics
import Network.URI
import Text.Casing (kebab)
import Text.Read (readMaybe)
import Web.Atomic.Types hiding (Selector)
import Web.Hyperbole.Data.Argument
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (formBody)
import Web.Hyperbole.Effect.Response (parseError)
import Web.Hyperbole.HyperView.Event (onSubmit)
import Web.Hyperbole.HyperView.Input (Option (..), checked)
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.Types.Request
import Web.Hyperbole.View


------------------------------------------------------------------------------
-- FORM PARSING
------------------------------------------------------------------------------

type ParamKey = BS.ByteString


{- | Simple types that be decoded from form data

@
#EMBED Example.FormSimple data ContactForm
@
-}
class FromForm (form :: Type) where
  fromForm :: Form -> Either String form
  default fromForm :: (Generic form, GFormParse (Rep form)) => Form -> Either String form
  fromForm f = to <$> gFormParse f


{- | A Higher-Kinded type that can be parsed from a 'Form'

@
#EMBED Example.FormValidation data UserForm
@
-}
class FromFormF (f :: (Type -> Type) -> Type) where
  fromFormF :: Form -> Either String (f Identity)
  default fromFormF :: (Generic (f Identity), GFormParse (Rep (f Identity))) => Form -> Either String (f Identity)
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


lookupParam :: ParamKey -> Form -> Maybe FormParam
lookupParam key f = FormParam . cs <$> lookup key f.params <|> FileParam <$> lookup key f.files


-- where
--  -- lookupFile = FileField <$> lookup key req.files
--  lookupParam = ParamField . ParamValue . cs <$>

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
  deriving newtype (Show, IsString, FromJSON, ToJSON)


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
instance (ViewId id, FromJSON id) => ViewId (Input id a) where
  type ViewState (Input id a) = ViewState id
  toViewId inp = toViewId inp.id


{- | label for a 'field'
label :: Text -> View (Input id a) ()
-}
label :: View c () -> View c ()
label = tag "label"


fileInput :: forall id a. View (Input id a) ()
fileInput = do
  inp :: Input id a <- viewId
  tag "input" @ att "type" "file" . name inp.inputName.value $ none


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
  tag "input" @ att "type" "checkbox" . checked isChecked . name inp.inputName.value $ none


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
instance (FromJSON id, FromJSON opt, ToJSON id, ToJSON opt) => ViewId (Radio id a opt) where
  type ViewState (Radio id a opt) = ViewState id


radioGroup :: opt -> View (Radio id a opt) () -> View (Input id a) ()
radioGroup defOpt = runChildView (\(inp :: Input id a) -> Radio inp.id inp.inputName defOpt)


radio :: forall id a opt. (Eq opt, ToJSON opt) => opt -> View (Radio id a opt) ()
radio val = do
  rd :: Radio id a opt <- viewId
  tag "input"
    @ att "type" "radio"
    . name rd.inputName.value
    . value (encodeArgument val)
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
-- FormField
------------------------------------------------------------------------------

class FromField a where
  fromField :: Maybe FormParam -> Either String a
  default fromField :: (FromJSON a) => Maybe FormParam -> Either String a
  fromField = argumentField


argumentField :: (FromJSON a) => Maybe FormParam -> Either String a
argumentField = \case
  Nothing -> Left "Missing form field value"
  Just (FormParam t) -> decodeArgument t
  Just (FileParam _) ->
    Left "Cannot parse uploaded file as param"


readField :: (Read a) => Maybe FormParam -> Either String a
readField = \case
  Nothing -> Left "Missing form field value"
  Just (FormParam p) -> do
    case readMaybe (cs p) of
      Nothing -> Left "Could not read form field"
      Just a -> pure a
  Just (FileParam _) ->
    Left "Cannot read uploaded file as param"


instance FromField Int where
  fromField Nothing = pure 0
  fromField (Just "") = pure 0
  fromField (Just t) = readField (Just t)
instance FromField Integer where
  fromField Nothing = pure 0
  fromField (Just "") = pure 0
  fromField (Just t) = readField (Just t)
instance FromField Float where
  fromField Nothing = pure 0
  fromField (Just "") = pure 0
  fromField (Just t) = readField (Just t)
instance FromField Double where
  fromField Nothing = pure 0
  fromField (Just "") = pure 0
  fromField (Just t) = readField (Just t)


instance FromField Bool where
  fromField Nothing = pure False
  fromField (Just (FileParam _)) = Left "Cannot parse file param as bool"
  fromField (Just (FormParam p)) =
    case p of
      "on" -> pure True
      "off" -> pure False
      "" -> pure False
      "false" -> pure False
      "true" -> pure True
      other -> Left $ "Could not parse bool form param: " <> cs other
instance FromField Char where
  fromField Nothing = Left "Missing form field value"
  fromField (Just (FileParam _)) = Left "Cannot parse file param as Char"
  fromField (Just (FormParam t)) =
    case cs t of
      (c : _) -> pure c
      _ -> Left "Could not parse empty form field as char"
instance FromField UTCTime where
  fromField = readField
instance FromField URI where
  fromField = \case
    Nothing -> Left "Missing form field value"
    Just (FileParam _) -> Left "Cannot parse file param as Char"
    Just (FormParam t) ->
      case parseURIReference (cs t) of
        Nothing -> Left "Invalid URI"
        Just a -> pure a
instance FromField String where
  fromField t = second cs $ fromField @Text t
instance FromField BS.ByteString where
  fromField t = second cs $ fromField @Text t
instance FromField Text where
  fromField = \case
    Nothing -> pure ""
    Just (FormParam t) -> pure t
    Just (FileParam _) -> Left "Cannot parse FileParam as Text"


instance {-# OVERLAPPABLE #-} (FromField a) => FromField (Maybe a) where
  fromField Nothing = pure Nothing
  fromField (Just "") = pure Nothing
  fromField (Just a) = Just <$> fromField @a (Just a)
instance {-# OVERLAPS #-} FromField (Maybe BS.ByteString) where
  fromField Nothing = pure Nothing
  fromField (Just "") = pure $ Just ""
  fromField (Just a) = Just <$> fromField @BS.ByteString (Just a)
instance {-# OVERLAPS #-} FromField (Maybe Text) where
  fromField Nothing = pure Nothing
  fromField (Just "") = pure $ Just ""
  fromField (Just a) = Just <$> fromField @Text (Just a)
instance {-# OVERLAPS #-} FromField (Maybe String) where
  fromField Nothing = pure Nothing
  fromField (Just "") = pure $ Just ""
  fromField (Just a) = Just <$> fromField @String (Just a)
instance {-# OVERLAPPABLE #-} (FromField a, FromField b) => FromField (Either a b) where
  fromField = \case
    Nothing -> Left "Missing form field value"
    Just p ->
      case fromField @a (Just p) of
        Right a -> pure $ Left a
        Left _ -> pure <$> fromField @b (Just p)
instance FromField UploadedFile where
  fromField = \case
    Nothing -> Left "Missing file upload"
    Just (FormParam "") -> Left "Missing file upload"
    Just (FormParam t) -> Left $ "Cannot parse FormParam as UploadedFile: " <> cs t
    Just (FileParam f) ->
      if f.fileName == mempty
        then Left "Empty file uploaded"
        else pure f
instance {-# OVERLAPS #-} FromField (Maybe UploadedFile) where
  fromField = \case
    Nothing -> pure Nothing
    Just (FileParam f) -> do
      if f.fileName == mempty
        then pure Nothing
        else pure $ Just f
    other -> fromField other


-- these instances don't make a lot of sense
-- instance (FromField a, FromField b) => FromField (a, b)
-- instance (FromField a, FromField b, FromField c) => FromField (a, b, c)
-- instance {-# OVERLAPPABLE #-} (FromParam a) => FromField [a]
-- instance FromField Value
-- instance FromField Word
-- instance FromField Word8
-- instance FromField Word16
-- instance FromField Word32
-- instance FromField Word64

------------------------------------------------------------------------------
-- GENERIC FORM PARSE
------------------------------------------------------------------------------

class GFormParse f where
  gFormParse :: Form -> Either String (f p)


instance (GFormParse f, GFormParse g) => GFormParse (f :*: g) where
  gFormParse f = do
    a <- gFormParse f
    b <- gFormParse f
    pure $ a :*: b


instance (GFormParse f) => GFormParse (M1 D d f) where
  gFormParse f = M1 <$> gFormParse f


instance (GFormParse f) => GFormParse (M1 C c f) where
  gFormParse f = M1 <$> gFormParse f


instance (Selector s, FromField a) => GFormParse (M1 S s (K1 R a)) where
  gFormParse f = do
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
    let mt :: Maybe FormParam = lookupParam (cs sel) f
    a <- first (\err -> sel <> ": " <> err) $ fromField @a mt
    pure $ M1 $ K1 a


-- class GFieldParse f where
--   gFieldParse :: String -> Maybe ParamValue -> Either String (f p)
--
--
-- instance {-# OVERLAPPABLE #-} (FromJSON a) => GFieldParse (K1 R a) where
--   gFieldParse sel Nothing = Left $ "Missing form field: " <> sel
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument t
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} (GFieldParse (K1 R a)) => GFieldParse (K1 R (Maybe a)) where
--   gFieldParse _ Nothing = pure $ K1 Nothing
--   gFieldParse _ (Just (ParamText "")) = pure $ K1 Nothing -- an empty string should be Nothing unless overriden
--   gFieldParse sel (Just p) = do
--     K1 a <- gFieldParse @(K1 R a) sel (Just p)
--     pure $ K1 $ Just a
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Bool) where
--   gFieldParse _ Nothing = pure $ K1 False
--   gFieldParse sel (Just t) = do
--     K1 <$> case t of
--       "on" -> pure True
--       "off" -> pure False
--       "" -> pure False
--       "false" -> pure False
--       "true" -> pure True
--       ParamText other -> Left $ "Could not parse bool field: " <> sel <> " = " <> cs other
--       ParamFile _ -> Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R (Maybe Text)) where
--   gFieldParse _ Nothing = pure $ K1 Nothing
--   gFieldParse sel inp = do
--     K1 t <- gFieldParse @(K1 R Text) sel inp
--     pure $ K1 $ Just t
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Text) where
--   gFieldParse _ Nothing = pure $ K1 ""
--   gFieldParse _ (Just "") = pure $ K1 ""
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument ("\"" <> t <> "\"")
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Int) where
--   gFieldParse _ Nothing = pure $ K1 0
--   gFieldParse _ (Just "") = pure $ K1 0
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument t
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Integer) where
--   gFieldParse _ Nothing = pure $ K1 0
--   gFieldParse _ (Just "") = pure $ K1 0
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument t
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Float) where
--   gFieldParse _ Nothing = pure $ K1 0
--   gFieldParse _ (Just "") = pure $ K1 0
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument t
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R Double) where
--   gFieldParse _ Nothing = pure $ K1 0
--   gFieldParse _ (Just "") = pure $ K1 0
--   gFieldParse _ (Just (ParamText t)) = K1 <$> decodeArgument t
--   gFieldParse sel (Just _) = Left $ "Unexpected file param: " <> sel
--
--
-- instance {-# OVERLAPS #-} GFieldParse (K1 R UploadedFile) where
--   gFieldParse sel Nothing = Left $ "Missing form field: " <> sel
--   gFieldParse _ (Just (ParamFile f)) = Right $ K1 f
--   gFieldParse sel (Just (ParamText _)) = Left $ "Unexpected text param: " <> sel

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
