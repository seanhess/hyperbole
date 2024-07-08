{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , placeholder
  , submit
  , formField
  , formFields
  , Form (..)
  , Field (..)
  , defaultFormOptions
  , FormOptions (..)
  , Validation
  , Validation' (..)
  , Validated (..)
  , FormField (..)
  , validation
  , fieldValid
  , anyInvalid
  , invalidText
  , validate
  , validateWith

    -- * Re-exports
  , FromHttpApiData
  , Generic
  )
where

import Data.Kind (Constraint, Type)
import Data.Text (Text, pack)
import Effectful
import GHC.Generics
import Text.Casing (kebab)
import Web.FormUrlEncoded qualified as FE
import Web.HttpApiData (FromHttpApiData (..))
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..), ViewId (..), dataTarget)
import Web.Hyperbole.Types (Elem)
import Web.Internal.FormUrlEncoded (FormOptions (..), defaultFormOptions)
import Web.View hiding (form, input, label)


-- | The only time we can use Fields is inside a form
data FormFields id v fs = FormFields id (Validation' v fs)


instance (ViewId id) => ViewId (FormFields id v fs) where
  parseViewId t = do
    i <- parseViewId t
    pure $ FormFields i mempty
  toViewId (FormFields i _) = toViewId i


instance (HyperView id, ViewId id) => HyperView (FormFields id v fs) where
  type Action (FormFields id v fs) = Action id


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


type Validation = Validation' Validated


newtype Validation' validated (fs :: [Type]) = Validation [(Text, validated ())]
  deriving newtype (Semigroup, Monoid)


instance (Show (v ())) => Show (Validation' v fs) where
  show (Validation v) = show v


validation :: forall a fs v. (FormField a, Elem a fs, ValidationState v, Monoid (v a)) => Validation' v fs -> v a
validation (Validation vs) = mconcat $ fmap (convert . snd) $ filter ((== inputName @a) . fst) vs


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
invalidText :: forall a fs id. (FormField a) => View (Input id Validated fs a) ()
invalidText = do
  Input _ v <- context
  case v of
    Invalid t -> text t
    _ -> none


-- | specify a check for a 'Validation'
validate :: forall a fs. (FormField a, Elem a fs) => Bool -> Text -> Validation fs
validate True t = Validation [(inputName @a, Invalid t)]
validate False _ = Validation [(inputName @a, NotInvalid)]


validateWith :: forall a fs v. (FormField a, Elem a fs, ValidationState v) => v a -> Validation' v fs
validateWith v = Validation [(inputName @a, convert v)]


anyInvalid :: Validation fs -> Bool
anyInvalid (Validation vs) =
  any (isInvalid . snd) vs


isInvalid :: Validated a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False


fieldValid :: View (Input id v fs a) (v a)
fieldValid = do
  Input _ v <- context
  pure v


data Label a


data Invalid a


data Input id v fs a = Input Text (v a)


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
field :: forall a id v fs. (FormField a, Elem a fs, ValidationState v, Monoid (v a)) => (v a -> Mod) -> View (Input id v fs a) () -> View (FormFields id v fs) ()
field f cnt = do
  let n = inputName @a
  FormFields _ vals <- context
  let v = validation @a vals
  tag "label" (f v . flexCol) $ do
    addContext (Input n v) cnt


-- | label for a 'field'
label :: Text -> View (Input id v fs a) ()
label = text


-- | input for a 'field'
input :: InputType -> Mod -> View (Input id v fs a) ()
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


placeholder :: Text -> Mod
placeholder = att "placeholder"


form' :: forall fs v id. (HyperView id) => Action id -> Validation' v fs -> Mod -> View (FormFields id v fs) () -> View id ()
form' a v f cnt = do
  vid <- context
  -- let frm = formLabels :: form Label
  -- let cnt = fcnt frm
  tag "form" (onSubmit a . dataTarget vid . f . flexCol) $ addContext (FormFields vid v) cnt
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
form :: forall fs id. (HyperView id) => Action id -> Validation fs -> Mod -> View (FormFields id Validated fs) () -> View id ()
form = form'


-- | Button that submits the 'form'. Use 'button' to specify actions other than submit
submit :: Mod -> View (FormFields id v fs) () -> View (FormFields id v fs) ()
submit f = tag "button" (att "type" "submit" . f)


-- type family Field' (context :: Type -> Type) a
-- type instance Field' Identity a = a
-- type instance Field' Label a = Text
-- type instance Field' Invalid a = Maybe Text

-- | Generic FormField type for parsing record-based forms
newtype Field a = Field {value :: a}


formFields :: forall form es. (Form form, Hyperbole :> es) => Eff es form
formFields = do
  f <- formData
  let ef = formParse f :: Either Text form
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
formField :: forall a es. (FormField a, Hyperbole :> es) => Eff es a
formField = do
  f <- formData
  case fieldParse f of
    Left e -> parseError e
    Right a -> pure a


class Form f where
  formParse :: FE.Form -> Either Text f
  default formParse :: (Generic f, GForm (Rep f)) => FE.Form -> Either Text f
  formParse f = to <$> gFormParse f


instance (FormField a, FormField b) => Form (a, b) where
  formParse f = do
    (,) <$> fieldParse f <*> fieldParse f


instance (FormField a, FormField b, FormField c) => Form (a, b, c) where
  formParse f = do
    (,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f


instance (FormField a, FormField b, FormField c, FormField d) => Form (a, b, c, d) where
  formParse f = do
    (,,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f


instance (FormField a, FormField b, FormField c, FormField d, FormField e) => Form (a, b, c, d, e) where
  formParse f = do
    (,,,,) <$> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f <*> fieldParse f


-- | Automatically derive labels from form field names
class GForm f where
  gFormParse :: FE.Form -> Either Text (f p)


-- instance GForm U1 where
--   gForm = U1

instance (GForm f, GForm g) => GForm (f :*: g) where
  gFormParse f = do
    a <- gFormParse f
    b <- gFormParse f
    pure $ a :*: b


instance (GForm f) => GForm (M1 D d f) where
  gFormParse f = M1 <$> gFormParse f


instance (GForm f) => GForm (M1 C c f) where
  gFormParse f = M1 <$> gFormParse f


instance {-# OVERLAPPABLE #-} (Selector s, GForm f) => GForm (M1 S s f) where
  gFormParse f = M1 <$> gFormParse f


instance (Selector s, FromHttpApiData a) => GForm (M1 S s (K1 R (Field a))) where
  gFormParse f =
    M1 . K1 . Field <$> do
      let sel = pack (selName (undefined :: M1 S s (K1 R a) p))
      FE.parseUnique sel f


instance (FormField a) => GForm (K1 R a) where
  gFormParse f = K1 <$> fieldParse f


-- instance GForm (M1 S s (K1 R (Maybe Text))) where
--   gForm = M1 . K1 $ Nothing

{- | Form Fields are identified by a type

@
data User = User Text deriving (Generic, FormField)
data Age = Age Int deriving (Generic, FormField)
@
-}
class FormField a where
  inputName :: Text
  default inputName :: (Generic a, GDataName (Rep a)) => Text
  inputName = gDataName (from (undefined :: a))


  fieldParse :: FE.Form -> Either Text a
  default fieldParse :: FE.Form -> Either Text a
  fieldParse = fieldParse' (inputName @a)


  fieldParse' :: Text -> FE.Form -> Either Text a
  default fieldParse' :: (Generic a, GFieldParse (Rep a)) => Text -> FE.Form -> Either Text a
  fieldParse' t f = to <$> gFieldParse t f


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

-------------------------------------------------
-- EXAMPLE --------------------------------------
-------------------------------------------------
-- data FormView = FormView
--   deriving (Generic, ViewId)
--
--
-- data FormAction = Submit
--   deriving (Generic, ViewAction)
--
--
-- instance HyperView FormView where
--   type Action FormView = FormAction
--
--
-- data User = User Text deriving (Generic, FormField)
-- data Age = Age Int deriving (Generic, FormField)
-- data Pass1 = Pass1 Text deriving (Generic, FormField)
-- data Pass2 = Pass2 Text deriving (Generic, FormField)
-- data FakeField = FakeField Text deriving (Generic, FormField)
--
--
-- type UserFields = [User, Age, Pass1, Pass2]
--
--
-- data ValidField a
--   = InvalidField Text
--   | ValidField a
--   | EmptyField
--   | NeedsCheck a
--
--
-- instance Semigroup (ValidField a) where
--   a <> _ = a
--
--
-- instance Monoid (ValidField a) where
--   mempty = EmptyField
--
--
-- instance ValidationState ValidField where
--   convert (InvalidField t) = InvalidField t
--   convert (ValidField a) = ValidField a
--   convert EmptyField = EmptyField
--   convert (NeedsCheck a) = NeedsCheck a
--

-- data UserForm = UserForm
--   { user :: User
--   , age :: Age
--   , pass1 :: Pass1
--   , pass2 :: Pass2
--   , woot :: Field Text
--   }
--   deriving (Generic, Form)

-- formAction :: (Hyperbole :> es) => FormView -> FormAction -> Eff es (View FormView ())
-- formAction _ Submit = do
--   u <- formField @User
--   a <- formField @Age
--   p1 <- formField @Pass1
--   p2 <- formField @Pass2
--
--   let vals = validateUser u a p1 p2
--   if anyInvalid vals
--     then pure $ formView vals
--     else pure $ userView u a p1
--
--
-- -- we don't type-check that we've validated all the fields here, but that's ok
-- validateUser :: User -> Age -> Pass1 -> Pass2 -> Validation UserFields
-- validateUser (User u) (Age a) (Pass1 p1) (Pass2 p2) =
--   mconcat
--     [ validate @Age (a < 20) "User must be at least 20 years old"
--     , validate @User (T.elem ' ' u) "Username must not contain spaces"
--     , validate @User (T.length u < 4) "Username must be at least 4 chars"
--     , validate @Pass1 (p1 /= p2) "Passwords did not match"
--     , validate @Pass1 (T.length p1 < 8) "Password must be at least 8 chars"
--     -- , validate @FakeField False "Bad"
--     ]
--
--
-- formView :: Validation' ValidField UserFields -> View FormView ()
-- formView v = do
--   form' Submit v (gap 10 . pad 10) $ do
--     el id "Sign Up"
--
--     field @User (const id) $ do
--       label "Username"
--       -- input Username (placeholder "username")
--       filledInput Username (placeholder "username")
--     -- el_ invalidText
--
--     field @Age (const id) $ do
--       label "Age"
--       filledInput Number (placeholder "age" . value "0")
--
--     field @Pass1 (const id) $ do
--       label "Password"
--       filledInput NewPassword (placeholder "password")
--
--     field @Pass2 (const id) $ do
--       label "Repeat Password"
--       filledInput NewPassword (placeholder "repeat password")
--
--
-- class ToValue a where
--   toValue :: a -> Text
--
--
-- instance ToValue User where
--   toValue (User u) = u
--
--
-- filledInput :: (ToValue a) => InputType -> Mod -> View (Input id ValidField a) ()
-- filledInput it f = do
--   v <- fieldValid
--   input it (f . val v)
--   case v of
--     InvalidField t -> el_ $ text $ "NOPE!" <> t
--     _ -> none
--  where
--   val (InvalidField _) = id
--   val EmptyField = id
--   val (NeedsCheck a) = value $ toValue a
--   val (ValidField a) = value $ toValue a
--
-- --
--
-- userView :: User -> Age -> Pass1 -> View FormView ()
-- userView (User user) (Age age) (Pass1 pass1) = do
--   el bold "Accepted Signup"
--   row (gap 5) $ do
--     el_ "Username:"
--     el_ $ text user
--
--   row (gap 5) $ do
--     el_ "Age:"
--     el_ $ text $ pack (show age)
--
