{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Input where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text
import Effectful
import GHC.Generics
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FE
import Web.Hyperbole

-- data Form a = Form

data Label a

-- newtype Label a = Label Text
--   deriving newtype (IsString)

-- something need to carry the field name in it
input' :: Text -> Mod -> View (form Label) ()
input' n f = tag "input" (f . name n) none

form' :: forall form action id. (Form form, HyperView action id) => action -> Mod -> (form Label -> View (form Label) ()) -> View id ()
form' a f fcnt = do
  vid <- context
  let frm = formLabels :: form Label
  let cnt = fcnt frm
  tag "form" (onSubmit a . dataTarget vid . f . flexCol) $ addContext frm cnt

parseForm :: (FromForm (form Identity), Hyperbole :> es) => Eff es (form Identity)
parseForm = parseFormData

class Form (form :: (Type -> Type) -> Type) where
  formLabels :: form Label
  default formLabels :: (Generic (form Label), GFormLabels (Rep (form Label))) => form Label
  formLabels = to gFormLabels

type family Field (context :: Type -> Type) a
type instance Field Identity a = a
type instance Field Label a = Text

-- Generic Form Labels
class GFormLabels f where
  gFormLabels :: f p

instance GFormLabels U1 where
  gFormLabels = U1

instance (GFormLabels f, GFormLabels g) => GFormLabels (f :*: g) where
  gFormLabels = gFormLabels :*: gFormLabels

instance (Selector s) => GFormLabels (M1 S s (K1 R Text)) where
  gFormLabels = M1 . K1 $ pack (selName (undefined :: M1 S s (K1 R Text) p))

instance (GFormLabels f) => GFormLabels (M1 D d f) where
  gFormLabels = M1 gFormLabels

instance (GFormLabels f) => GFormLabels (M1 C c f) where
  gFormLabels = M1 gFormLabels
