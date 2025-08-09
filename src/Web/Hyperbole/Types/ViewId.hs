module Web.Hyperbole.Types.ViewId where


import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Effectful
import Effectful.Reader.Dynamic
import GHC.Generics
import GHC.TypeLits hiding (Mod)
import Web.Atomic.CSS
import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.TypeList
import Web.Hyperbole.View (View, addContext, context, none, tag)


class ViewId a where
  toViewId :: a -> Encoded
  default toViewId :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toViewId = genericToEncoded


  parseViewId :: Encoded -> Either Text a
  default parseViewId :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either Text a
  parseViewId = genericParseEncoded


{- | Access the 'viewId' in a 'View' or 'update'

@
#EMBED Example/Page/LazyLoading.hs data LazyData

#EMBED Example/Page/LazyLoading.hs instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
@
-}
class HasViewId m view where
  viewId :: m view


instance HasViewId (View ctx) ctx where
  viewId = context
instance HasViewId (Eff (Reader view : es)) view where
  viewId = ask
