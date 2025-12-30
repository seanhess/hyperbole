{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.View.ViewId where

import Data.Kind (Type)
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded


{- | A unique identifier for a 'HyperView'

@
#EMBED Example/Page/Simple.hs data Message
@
-}
class ViewId a where
  type ViewState a :: Type
  type ViewState a = ()


  toViewId :: a -> Encoded
  default toViewId :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toViewId = genericToEncoded


  parseViewId :: Encoded -> Either String a
  default parseViewId :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either String a
  parseViewId = genericParseEncoded


instance ViewId () where
  toViewId _ = mempty
  parseViewId _ = pure ()
