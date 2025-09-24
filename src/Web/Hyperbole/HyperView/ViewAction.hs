{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.HyperView.ViewAction where

import Data.Text (Text)
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded


class ViewAction a where
  toAction :: a -> Encoded
  default toAction :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toAction = genericToEncoded


  parseAction :: Encoded -> Either String a
  default parseAction :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either String a
  parseAction = genericParseEncoded


instance ViewAction () where
  toAction _ = mempty
  parseAction _ = pure ()


encodeAction :: (ViewAction act) => act -> Text
encodeAction = encodedToText . toAction


decodeAction :: (ViewAction act) => Text -> Maybe act
decodeAction t = do
  case parseAction =<< encodedParseText t of
    Left _ -> Nothing
    Right a -> pure a
