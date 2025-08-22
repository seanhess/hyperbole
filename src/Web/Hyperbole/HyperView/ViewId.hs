{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.HyperView.ViewId where

import Data.Text (Text)
import Effectful
import Effectful.Reader.Dynamic
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.View (View, context)


class ViewId a where
  toViewId :: a -> Encoded
  default toViewId :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toViewId = genericToEncoded


  parseViewId :: Encoded -> Either Text a
  default parseViewId :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either Text a
  parseViewId = genericParseEncoded


{- | Access the 'viewId' in a 'View' or 'update'

@
#EMBED Example/Page/Concurrency.hs data LazyData

#EMBED Example/Page/Concurrency.hs instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
@
-}
class HasViewId m view where
  viewId :: m view


instance HasViewId (View ctx) ctx where
  viewId = context
instance HasViewId (Eff (Reader view : es)) view where
  viewId = ask


encodeViewId :: (ViewId id) => id -> Text
encodeViewId = encodedToText . toViewId


decodeViewId :: (ViewId id) => Text -> Maybe id
decodeViewId t = do
  case parseViewId =<< encodedParseText t of
    Left _ -> Nothing
    Right a -> pure a
