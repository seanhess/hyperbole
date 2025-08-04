{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Data.Param where

import Data.Aeson as A (FromJSON (..), Result (..), ToJSON (..), Value (..), eitherDecode, encode, fromJSON)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Word
import Network.HTTP.Types (urlDecode, urlEncode)
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)
import Web.Hyperbole.Data.URI (URI (..), parseURIReference, uriToText)


newtype Param = Param {text :: Text}
  deriving newtype (Show, Eq, Ord, IsString)


newtype ParamValue = ParamValue {text :: Text}
  deriving newtype (Show, Eq, IsString)


{- | 'session's, 'form's, and 'query's all encode data as query strings. ToParam and FromParam control how a datatype is encoded to a parameter.
 -
This is equivalent to Web.HttpApiData, which is missing some instances and has some strange defaults

@
#EMBED Example/Effects/Todos.hs data Todo = Todo
@

@
#EMBED Example/Docs/Encoding.hs data Tags

#EMBED Example/Docs/Encoding.hs instance ToParam Tags
@
-}
class ToParam a where
  toParam :: a -> ParamValue
  default toParam :: (ToJSON a) => a -> ParamValue
  toParam = jsonParam


instance ToParam Int where
  toParam = ParamValue . toQueryParam
instance ToParam Integer where
  toParam = ParamValue . toQueryParam
instance ToParam Text where
  toParam = ParamValue . toQueryParam
instance ToParam Float where
  toParam = ParamValue . toQueryParam
instance ToParam Double where
  toParam = ParamValue . toQueryParam
instance ToParam Word where
  toParam = ParamValue . toQueryParam
instance ToParam Word8 where
  toParam = ParamValue . toQueryParam
instance ToParam Word16 where
  toParam = ParamValue . toQueryParam
instance ToParam Word32 where
  toParam = ParamValue . toQueryParam
instance ToParam Word64 where
  toParam = ParamValue . toQueryParam
instance ToParam Bool where
  toParam = ParamValue . toQueryParam
instance ToParam Char where
  toParam = ParamValue . toQueryParam
instance ToParam UTCTime where
  toParam = ParamValue . toQueryParam
instance ToParam URI where
  toParam = toParam . uriToText


{- | Decode data from a 'query', 'session', or 'form' parameter value

@
#EMBED Example/Effects/Todos.hs data Todo = Todo
@

@
#EMBED Example/Docs/Encoding.hs data Tags

#EMBED Example/Docs/Encoding.hs instance FromParam Tags
@
-}
class FromParam a where
  parseParam :: ParamValue -> Either Text a
  default parseParam :: (FromJSON a) => ParamValue -> Either Text a
  parseParam = jsonParse


-- What is the default instance?
instance FromParam Int where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Integer where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Float where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Text where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Double where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Word where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Word8 where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Word16 where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Word32 where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Word64 where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Bool where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam Char where
  parseParam (ParamValue t) = parseQueryParam t
instance FromParam UTCTime where
  parseParam (ParamValue t) = parseQueryParam t


instance FromParam URI where
  parseParam (ParamValue t) = do
    case parseURIReference (cs t) of
      Nothing -> Left $ "Invalid URI: " <> t
      Just u -> pure u


-- these are NOT escaped yet
instance (ToParam a) => ToParam [a] where
  toParam as = ParamValue $ T.intercalate "+" $ fmap (cs . urlEncode True . cs . (.text) . toParam) as
instance (FromParam a) => FromParam [a] where
  parseParam (ParamValue t) = do
    let parts = T.splitOn "+" $ cs t
    mapM (parseParam . ParamValue . cs . urlDecode True . cs) parts


instance (ToParam a) => ToParam (Maybe a) where
  toParam Nothing = ParamValue ""
  toParam (Just a) = toParam a
instance (FromParam a) => FromParam (Maybe a) where
  parseParam (ParamValue "") = pure Nothing
  parseParam t = Just <$> parseParam @a t


instance (ToParam a, ToParam b) => ToParam (Either a b) where
  toParam (Left a) = toParam a
  toParam (Right b) = toParam b
instance (FromParam a, FromParam b) => FromParam (Either a b) where
  parseParam val =
    case parseParam @a val of
      Right a -> pure $ Left a
      Left _ -> do
        case parseParam @b val of
          Left _ -> Left $ "Could not parseParam Either: " <> val.text
          Right b -> pure $ Right b


-- -- -- | Encode a Show as a query param
showParam :: (Show a) => a -> ParamValue
showParam a = ParamValue $ toQueryParam $ show a


-- | Decode a Read as a query param
readParam :: (Read a) => ParamValue -> Either Text a
readParam (ParamValue t) = do
  str <- parseQueryParam t
  case readMaybe str of
    Nothing -> Left $ cs $ "Could not read query param: " <> str
    Just a -> pure a


jsonParam :: (ToJSON a) => a -> ParamValue
jsonParam a =
  case toJSON a of
    String t -> toParam t
    val -> ParamValue $ cs $ A.encode val


jsonParse :: (FromJSON a) => ParamValue -> Either Text a
jsonParse (ParamValue t) = do
  fromResult $ fromJSON parseValue
 where
  parseValue :: Value
  parseValue = do
    case A.eitherDecode @Value (cs t) of
      Left _ -> String t
      Right val -> val

  fromResult :: Result a -> Either Text a
  fromResult = \case
    Success a -> pure a
    Error e -> Left (cs e)


-- | Parse a Traversable (list) of params
parseParams :: (Traversable t, FromParam a) => t ParamValue -> Either Text (t a)
parseParams = traverse parseParam


newtype HttpApiData a = HttpApiData {value :: a}
instance (ToHttpApiData a) => ToParam (HttpApiData a) where
  toParam (HttpApiData a) = ParamValue $ toQueryParam a
instance (FromHttpApiData a) => FromParam (HttpApiData a) where
  parseParam (ParamValue t) = HttpApiData <$> parseQueryParam t
