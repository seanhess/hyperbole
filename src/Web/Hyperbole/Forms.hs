{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Forms
  ( FormFields (..)
  , InputType (..)
  , FieldName
  , Invalid
  , Input (..)
  , field
  , label
  , input
  , form
  , placeholder
  , submit
  , formFields
  , Form (..)
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
  , FromHttpApiData
  , Generic
  , GenFields (..)
  , GenField (..)
  , test
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.Text (Text, pack)
import Effectful
import GHC.Generics
import GHC.TypeLits hiding (Mod)
import Text.Casing (kebab)
import Web.FormUrlEncoded qualified as FE
import Web.HttpApiData (FromHttpApiData (..))
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..), ViewId (..), dataTarget)
import Web.Internal.FormUrlEncoded (FormOptions (..), defaultFormOptions, genericFromForm, parseUnique)
import Web.View hiding (form, input, label)


-- | The only time we can use Fields is inside a form
data FormFields id v form = FormFields id (form (FormField v))


data FormField v a = FormField
  { label :: FieldName a
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


{- | Validation results for a 'form'

@
validateUser :: User -> Age -> Validation
validateUser (User u) (Age a) =
  validation
    [ 'validate' \@Age (a < 20) "User must be at least 20 years old"
    , 'validate' \@User (T.elem ' ' u) "Username must not contain spaces"
    , 'validate' \@User (T.length u < 4) "Username must be at least 4 chars"
    ]

formAction :: ('Hyperbole' :> es, 'UserDB' :> es) => FormView -> FormAction -> 'Eff' es ('View' FormView ())
formAction _ SignUp = do
  a <- 'formField' \@Age
  u <- 'formField' \@User

  case validateUser u a of
    'Validation' [] -> successView
    errs -> userForm v
@
@
-}

-- would be easier if you pass in your own data. Right now everything is indexed by type
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


-- type Validation = Validation' Validated
--
--
-- newtype Validation' validated a = Validation [(Text, validated ())]
--   deriving newtype (Semigroup, Monoid)

-- instance (Show (v ())) => Show (Validation' v fs) where
--   show (Validation v) = show v
--
--
-- validation :: forall a fs v. (FormField a, Elem a fs, ValidationState v, Monoid (v a)) => Validation' v fs -> v a
-- validation (Validation vs) = mconcat $ fmap (convert . snd) $ filter ((== inputName @a) . fst) vs

class ValidationState (v :: Type -> Type) where
  convert :: v a -> v b


instance ValidationState Validated where
  convert :: Validated a -> Validated b
  convert (Invalid t) = Invalid t
  convert NotInvalid = NotInvalid
  convert Valid = Valid


{-
@
'field' \@User id Style.invalid $ do
  'label' \"Username\"
  'input' Username ('placeholder' "username")
  el_ 'invalidText'
@
-}
invalidText :: forall a fs id. View (Input id Validated fs a) ()
invalidText = do
  Input _ v <- context
  case v of
    Invalid t -> text t
    _ -> none


-- | specify a check for a 'Validation'
validate :: Bool -> Text -> Validated a
validate True t = Invalid t -- Validation [(inputName @a, Invalid t)]
validate False _ = NotInvalid -- Validation [(inputName @a, NotInvalid)]


-- validateWith :: forall a fs v. (FormField a, Elem a fs, ValidationState v) => v a -> Validation' v fs
-- validateWith v = Validation [(inputName @a, convert v)]

-- eh... not sure how to do this...
anyInvalid :: form Validated -> Bool
anyInvalid _ = _


-- any (isInvalid . snd) vs

isInvalid :: Validated a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False


fieldValid :: View (Input id v fs a) (v a)
fieldValid = do
  Input _ v <- context
  pure v


data FieldName a = FieldName Text
  deriving (Show)


data Invalid a


data Input id v fs a = Input
  { inputName :: FieldName a
  , valid :: v a
  }


{- | Display a 'FormField'

@
data Age = Age Int deriving (Generic, FormField)

myForm = do
  'form' SignUp mempty id $ do
    field @Age id id $ do
     'label' "Age"
     'input' Number (value "0")
@
-}
field
  :: forall a id f v form
   . (ValidationState v, Monoid (v a))
  => (form (FormField v) -> FormField v a)
  -> (v a -> Mod)
  -> View (Input id v form a) ()
  -> View (FormFields id v form) ()
field toField md cnt = do
  FormFields _ frm <- context
  let fld = toField frm :: FormField v a
  tag "label" (md fld.validated . flexCol) $ do
    addContext (Input fld.label fld.validated) cnt


-- | label for a 'field'
label :: Text -> View (Input id v fs a) ()
label = text


-- | input for a 'field'
input :: InputType -> Mod -> View (Input id v fs a) ()
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


placeholder :: Text -> Mod
placeholder = att "placeholder"


form' :: forall form v id. (HyperView id) => Action id -> form v -> Mod -> View (FormFields id v form) () -> View id ()
form' a v f cnt = do
  vid <- context
  frm <- pure $ _ v :: View id (form (FormField v))

  -- TODO: need a generic way to merge all fields.
  --  (Form form) => mapFields (\v selName -> FormField selName v)

  -- let frm = formLabels :: form Label
  -- let cnt = fcnt frm
  tag "form" (onSubmit a . dataTarget vid . f . flexCol) $ addContext (FormFields vid _) cnt
 where
  onSubmit :: (ViewAction a) => a -> Mod
  onSubmit = att "data-on-submit" . toAction


{- | Type-safe \<form\>. Calls (Action id) on submit

@
userForm :: 'Validation' -> 'View' FormView ()
userForm v = do
  form Signup v id $ do
    el Style.h1 "Sign Up"

    'field' \@User id Style.invalid $ do
      'label' \"Username\"
      'input' Username ('placeholder' "username")
      el_ 'invalidText'

    'field' \@Age id Style.invalid $ do
      'label' \"Age\"
      'input' Number ('placeholder' "age" . value "0")
      el_ 'invalidText'

    'submit' (border 1) \"Submit\"
@
-}
form :: forall form id. (HyperView id) => Action id -> form Validated -> Mod -> View (FormFields id Validated form) () -> View id ()
form = form'


-- | Button that submits the 'form'. Use 'button' to specify actions other than submit
submit :: Mod -> View (FormFields id v fs) () -> View (FormFields id v fs) ()
submit f = tag "button" (att "type" "submit" . f)


type family Field (context :: Type -> Type) a
type instance Field Identity a = a
type instance Field FieldName a = FieldName a
type instance Field (FormField v) a = FormField v a
type instance Field Validated a = Validated a


formFields :: forall form es. (Form form, Hyperbole :> es) => Eff es (form Identity)
formFields = do
  f <- formData
  let ef = formParse f :: Either Text (form Identity)
  either parseError pure ef


{- | Parse a 'FormField' from the request

@
formAction :: ('Hyperbole' :> es, 'UserDB' :> es) => FormView -> FormAction -> 'Eff' es ('View' FormView ())
formAction _ SignUp = do
  a <- formField \@Age
  u <- formField \@User
  saveUserToDB u a
  pure $ el_ "Saved!"
@
-}

-- formField :: forall a es. (FormField a, Hyperbole :> es) => Eff es a
-- formField = do
--   f <- formData
--   case fieldParse f of
--     Left e -> parseError e
--     Right a -> pure a

class Form form where
  formParse :: FE.Form -> Either Text (form Identity)
  default formParse :: (Generic (form Identity), GFormParse (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
  formParse f = to <$> gFormParse f


  fieldsConvert :: form f -> form g
  default fieldsConvert :: (Generic (form f), Generic (form g), GConvert (Rep (form f)) (Rep (form g))) => form f -> form g
  fieldsConvert x = to $ gConvert (from x)


  fieldNames :: form FieldName
  default fieldNames :: (Generic (form FieldName), GenFields (Rep (form FieldName))) => form FieldName
  fieldNames = to gGenFields


  fieldValids :: form Validated
  default fieldValids :: (Generic (form Validated), GenFields (Rep (form Validated))) => form Validated
  fieldValids = to gGenFields


-- fromForm :: FE.Form -> Either Text (form Identity)
-- default fromForm :: (Generic (form Identity), GFromForm (form Identity) (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
-- fromForm = genericFromForm defaultFormOptions

-- formFieldNames :: f (FieldName
-- formFieldNames = _

-- instance (FormField a, FormField b) => Form (a, b) where
--   formParse f = do
--     (,) <$> fieldParse f <*> fieldParse f
--
--
-- instance (FormField a, FormField b, FormField c) => Form (a, b, c) where
--   formParse f = do
--     (,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f
--
--
-- instance (FormField a, FormField b, FormField c, FormField d) => Form (a, b, c, d) where
--   formParse f = do
--     (,,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f
--
--
-- instance (FormField a, FormField b, FormField c, FormField d, FormField e) => Form (a, b, c, d, e) where
--   formParse f = do
--     (,,,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f

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


instance (Selector s, FromHttpApiData a) => GFormParse (M1 S s (K1 R a)) where
  gFormParse f =
    let s = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 <$> parseUnique (pack s) f


------------------------------------------------------------------------------
-- GEN FIELDS :: Create the field! -------------------------------------------
------------------------------------------------------------------------------

class GenFields f where
  gGenFields :: f p


instance GenFields U1 where
  gGenFields = U1


instance (GenFields f, GenFields g) => GenFields (f :*: g) where
  gGenFields = gGenFields :*: gGenFields


instance (Selector s, GenField f a) => GenFields (M1 S s (K1 R (f a))) where
  gGenFields = M1 . K1 $ genField (selName (undefined :: M1 S s (K1 R (f a)) p))


instance (GenFields f) => GenFields (M1 D d f) where
  gGenFields = M1 gGenFields


instance (GenFields f) => GenFields (M1 C c f) where
  gGenFields = M1 gGenFields


------------------------------------------------------------------------------
-- GenField -- Generate a value from the selector name
------------------------------------------------------------------------------

class GenField f a where
  genField :: String -> f a


instance GenField FieldName a where
  genField s = FieldName $ pack s


instance GenField Validated a where
  genField = const NotInvalid


instance GenField (FormField Validated) a where
  genField s = FormField (genField s) NotInvalid


------------------------------------------------------------------------------
-- ConvertFields: start with another one
------------------------------------------------------------------------------

-- class ConvertFields a where
--   convertFields :: (FromSelector f g) => a f -> a g
--   default convertFields :: (Generic (a f), Generic (a g), GConvert (Rep (a f)) (Rep (a g))) => a f -> a g
--   convertFields x = to $ gConvert (from x)

class GConvert rf rg where
  gConvert :: rf p -> rg p


instance (GConvert a1 a2, GConvert b1 b2) => GConvert (a1 :*: b1) (a2 :*: b2) where
  gConvert (a :*: b) = gConvert a :*: gConvert b


instance (GConvert f g) => GConvert (M1 D d f) (M1 D d g) where
  gConvert (M1 fa) = M1 $ gConvert fa


instance (GConvert f g) => GConvert (M1 C d f) (M1 C d g) where
  gConvert (M1 fa) = M1 $ gConvert fa


instance (Selector s, FromSelector f g a) => GConvert (M1 S s (K1 R (f a))) (M1 S s (K1 R (g a))) where
  gConvert (M1 (K1 fa)) =
    let sel = selName (undefined :: M1 S s (K1 R (f a)) p)
     in M1 . K1 $ fromSelector sel fa


class FromSelector f g a where
  fromSelector :: String -> f a -> g a


-- if we start with validateds, we can add the names!
instance FromSelector v (FormField v) a where
  fromSelector s = FormField (FieldName $ pack s)


data MyType f = MyType {one :: Field f Int, two :: Field f Text}
  deriving (Generic, Form)


test :: IO ()
test = do
  let formValids = fieldValids :: MyType Validated
      formNames = fieldNames :: MyType FieldName
      vs = MyType{one = NotInvalid, two = Invalid "NOPPE"} :: MyType Validated
      b = formValids
      c = formNames
  -- a = convertFields formNames :: MyType (FormField Validated)
  -- b = fieldsConvert formValids :: MyType (FormField Validated)
  -- c = fieldsConvert vs :: MyType (FormField Validated)

  -- print (a.one, a.two)
  print (b.one, b.two)
  print (c.one, c.two)
