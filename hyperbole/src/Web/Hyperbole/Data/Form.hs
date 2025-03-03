{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.Form
  ( -- * Form Parsing
    FromForm (..)
  , formParam
  , formLookup
  , formData

    -- * Higher Kinded Forms
  , FromFormF (..)
  , Field

    -- * Generate Fields
  , GenFields (..)
  , GenField (..)
  , fieldNames
  , FieldName (..)

    -- * Validation
  , Validated (..)
  , isInvalid
  , validate

    -- * Exports

    -- ** Generics
  , GFieldsGen (..)
  , GFormParse (..)
  , genericFromForm

    -- ** Re-exports
  , Identity
  , Generic
  , Form (..)
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import GHC.Generics
import Web.FormUrlEncoded (Form (..), parseUnique)
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole.Data.QueryData (FromParam (..), Param (..), ParamValue (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response (parseError)


------------------------------------------------------------------------------
-- FORM PARSING
------------------------------------------------------------------------------

{- | Equivalent to Web.FormUrlEncoded.FromForm, but the generic implementation uses 'FromParam' instead of 'FromHttpApiData'

@
#EMBED Example/Page/FormSimple.hs data ContactForm
@
-}
class FromForm (form :: Type) where
  fromForm :: FE.Form -> Either Text form
  default fromForm :: (Generic form, GFormParse (Rep form)) => FE.Form -> Either Text form
  fromForm = genericFromForm


{- | We can use Higher Kinded Types to make Forms more convenient. Practically, a Higher Kinded Form is a record with the same (f :: Type -> Type) type variable applied to each field, which makes it easy to use the same record structure for different data. For example:

@
#EMBED Example/Page/FormSimple.hs data ContactForm'
@

The same record can then be used to represent the same fields with many different (f :: Type -> Type)

> contact :: ContactForm Identity
> contact = ContactForm "Bob" 44

> contactFields :: ContactForm FieldName
> contactFields = ContactForm (FieldName "name") (FieldName "age")

> contactValues :: ContactForm Maybe
> contactValues = ContactForm Nothing (Just 44)
-}
class FromFormF (form :: (Type -> Type) -> Type) where
  fromFormF :: FE.Form -> Either Text (form Identity)
  default fromFormF :: (Generic (form Identity), GFormParse (Rep (form Identity))) => FE.Form -> Either Text (form Identity)
  fromFormF = genericFromForm


-- Any FromFormF can be parsed using fromForm @(form Identity)
instance (FromFormF form) => FromForm (form Identity) where
  fromForm = fromFormF


{- | Parse a full type from a form url-encoded body

@
#EMBED Example/Page/Todo.hs   update (SubmitEdit todo) = do
@
-}
formData :: forall form es. (FromForm form, Hyperbole :> es) => Eff es form
formData = do
  f <- formBody
  let ef = fromForm @form f :: Either Text form
  either parseError pure ef


-- | Parse a single param from a Form
formParam :: (FromParam a) => Param -> FE.Form -> Either Text a
formParam (Param key) frm = do
  t <- FE.parseUnique @Text key frm
  parseParam (ParamValue t)


-- | Lookup a single param from a Form
formLookup :: (FromParam a) => Param -> FE.Form -> Either Text (Maybe a)
formLookup (Param key) frm = do
  mt <- FE.parseMaybe @Text key frm
  maybe (pure Nothing) (parseParam . ParamValue) mt


------------------------------------------------------------------------------
-- GEN FIELDS: Generate a type from selector names
------------------------------------------------------------------------------

{- | Field allows a Higher Kinded Form to resolve different f variables. Specifically:

> Field Identity a ~ a
> Field f a ~ f a
-}
type family Field (functor :: Type -> Type) a where
  Field Identity a = a
  Field f a = f a


{- | Generate a Higher Kinded Type (form f)

@
#EMBED Example/Page/FormValidation.hs data UserForm

#EMBED Example/Page/FormValidation.hs page
@
-}
class GenFields f (form :: (Type -> Type) -> Type) where
  genFields :: form f
  default genFields :: (Generic (form f), GFieldsGen (Rep (form f))) => form f
  genFields = to gFieldsGen


-- Given a selector, generate the type
class GenField a where
  genField :: String -> a


instance GenField (Validated a) where
  genField = const NotInvalid


instance GenField (FieldName a) where
  genField s = FieldName $ pack s


instance GenField (Maybe a) where
  genField _ = Nothing


-- | Form FieldName. This is embeded as the name attribute, and refers to the key need to parse the form when submitted. See 'fieldNames'
newtype FieldName a = FieldName Text
  deriving newtype (Show, IsString)


{- | Generate FieldNames for a form. See [Example.Page.FormSimple](https://docs.hyperbole.live/formsimple)

> #EMBED Example/Page/FormSimple.hs data ContactForm'
>
> #EMBED Example/Page/FormSimple.hs formView'
-}
fieldNames :: forall form. (GenFields FieldName form) => form FieldName
fieldNames = genFields


------------------------------------------------------------------------------
-- VALIDATION
------------------------------------------------------------------------------

{- | Validation results for a 'Form'. See 'validate'

@
#EMBED Example/Page/FormValidation.hs data UserForm

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

{- | specify a check for a 'Validation'

@
#EMBED Example/Page/FormValidation.hs validateAge
@
-}
validate :: Bool -> Text -> Validated a
validate True t = Invalid t -- Validation [(inputName @a, Invalid t)]
validate False _ = NotInvalid -- Validation [(inputName @a, NotInvalid)]


------------------------------------------------------------------------------
-- GENERIC FORM PARSE
------------------------------------------------------------------------------

genericFromForm :: (Generic form, GFormParse (Rep form)) => FE.Form -> Either Text form
genericFromForm f = to <$> gFormParse f


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

------------------------------------------------------------------------------

newtype User = User Text
  deriving newtype (FromParam)


data TestForm f = TestForm
  { name :: Field f Text
  , age :: Field f Int
  , user :: Field f User
  }
  deriving (Generic, FromFormF, GenFields Maybe, GenFields Validated)

-- test :: (Hyperbole :> es) => Eff es (TestForm Identity)
-- test = do
--   tf <- formData
--   pure tf

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
