{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.View.ViewId where

import Data.Kind (Type)
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded


{- ! A unique identifier for a 'HyperView'

@
#EMBED Example.Simple data Message
@
-}

{- | A unique identifier for a 'HyperView'

@
data Message = Message1 | Message2
  deriving (Generic, 'ViewId')
@
-}
class ViewId a where
  -- | Most hyperviews are stateless, but you can track internal state by setting this to something other than `()`
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
