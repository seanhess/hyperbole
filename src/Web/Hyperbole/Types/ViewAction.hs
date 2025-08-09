module Web.Hyperbole.Types.ViewAction where

import Data.Text (Text)
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded


class ViewAction a where
  toAction :: a -> Encoded
  default toAction :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toAction = genericToEncoded


  parseAction :: Encoded -> Either Text a
  default parseAction :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either Text a
  parseAction = genericParseEncoded


instance ViewAction () where
  toAction _ = mempty
  parseAction _ = pure ()
