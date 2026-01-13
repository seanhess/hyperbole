{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Data.Param where

import Data.Aeson (FromJSON, GFromJSON, GToJSON, Options (..), SumEncoding (..), ToJSON, Value (..), Zero, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Bifunctor (first)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word
import GHC.Exts (IsList (..))
import GHC.Generics
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.HttpApiData qualified as HttpApiData
import Web.Hyperbole.Data.URI (URI (..), parseURIReference, uriToText)


newtype Param = Param {text :: Text}
  deriving newtype (Show, Eq, Ord, IsString)


-- | Encode arbitrarily complex data into url form encoded data
newtype ParamValue = ParamValue {value :: Text}
  deriving newtype (Ord, Eq)
  deriving (Show, Generic)


instance IsString ParamValue where
  fromString s = ParamValue (cs s)


{- | 'session's, 'form's, and 'query's all encode data as query strings. ToParam and FromParam control how a datatype is encoded to a parameter.
 -
This is equivalent to Web.HttpApiData, which is missing some instances and has some strange defaults

@
#EMBED Example.Docs.Sessions data AppColor
@
-}
class ToParam a where
  toParam :: a -> ParamValue
  default toParam :: (Generic a, GToJSON Zero (Rep a)) => a -> ParamValue
  toParam = genericToParam


instance ToParam ParamValue where
  toParam = id
instance ToParam Int where
  toParam = jsonParam
instance ToParam Integer where
  toParam = jsonParam
instance ToParam Text where
  toParam = toQueryParam
instance {-# OVERLAPS #-} ToParam String where
  toParam = toQueryParam
instance ToParam Float where
  toParam = jsonParam
instance ToParam Double where
  toParam = jsonParam
instance ToParam Word where
  toParam = jsonParam
instance ToParam Word8 where
  toParam = jsonParam
instance ToParam Word16 where
  toParam = jsonParam
instance ToParam Word32 where
  toParam = jsonParam
instance ToParam Word64 where
  toParam = jsonParam
instance ToParam Bool where
  toParam = jsonParam
instance ToParam Char where
  toParam = toQueryParam
instance ToParam UTCTime where
  toParam = toQueryParam
instance ToParam URI where
  toParam = toParam . uriToText
instance ToParam Value where
  toParam = jsonParam
instance (ToParam a, ToParam b) => ToParam (a, b) where
  toParam (a, b) = toParam [toParam a, toParam b]


{- | Decode data from a 'query', 'session', or 'form' parameter value

@
#EMBED Example.Docs.Sessions data AppColor
@
-}
class FromParam a where
  parseParam :: ParamValue -> Either String a
  default parseParam :: (Generic a, GFromJSON Zero (Rep a)) => ParamValue -> Either String a
  parseParam = genericParseParam


  decodeFormValue :: Maybe Text -> Either String a
  decodeFormValue mval = do
    case mval of
      Nothing -> Left "missing form field value"
      Just t -> do
        parseParam $ ParamValue t


-- decodeParamValue :: Text -> Either String a
-- decodeParamValue = parseParam . decodeParam

-- Permissive instances. Some of these come directly from forms!
instance FromParam ParamValue where
  parseParam = pure
instance FromParam Int where
  parseParam "" = pure 0
  parseParam p = jsonParse p
instance FromParam Integer where
  parseParam "" = pure 0
  parseParam p = jsonParse p
instance FromParam Float where
  parseParam "" = pure 0
  parseParam p = jsonParse p
instance FromParam Double where
  parseParam "" = pure 0
  parseParam p = jsonParse p
instance FromParam Text where
  parseParam = parseQueryParam
instance (FromParam a, FromParam b) => FromParam (a, b) where
  parseParam p = do
    ps <- parseParam @[ParamValue] p
    case ps of
      [pa, pb] -> (,) <$> parseParam pa <*> parseParam pb
      _ -> Left $ "Expected [a,b] but got: " <> cs p.value


-- -- we don't need to desanitize the text
-- parseFormField [inp] = do
--   parseParam $ ParamValue inp (String inp)

instance {-# OVERLAPS #-} FromParam String where
  parseParam p = cs <$> parseQueryParam @Text p


-- parseFormField sel f = do
--   inp :: Text <- first cs $ FE.parseUnique @Text (cs sel) f
--   parseParam $ ParamValue inp (String inp)
instance FromParam Word where
  parseParam = jsonParse
instance FromParam Word8 where
  parseParam = jsonParse
instance FromParam Word16 where
  parseParam = jsonParse
instance FromParam Word32 where
  parseParam = jsonParse
instance FromParam Word64 where
  parseParam = jsonParse
instance FromParam Bool where
  parseParam (ParamValue t) =
    case t of
      "on" -> pure True
      "off" -> pure False
      "" -> pure False
      "false" -> pure False
      "true" -> pure True
      other -> Left $ "Could not parse bool param: " <> cs other


  decodeFormValue Nothing = pure False
  decodeFormValue (Just t) =
    parseParam $ ParamValue t


instance FromParam Char where
  parseParam = parseQueryParam
instance FromParam UTCTime where
  parseParam = parseQueryParam
instance FromParam Value where
  parseParam = jsonParse


instance FromParam URI where
  parseParam (ParamValue t) = do
    case parseURIReference (cs t) of
      Nothing -> Left $ "Invalid URI: " <> cs t
      Just u -> pure u


instance {-# OVERLAPPABLE #-} (ToParam a) => ToParam [a] where
  toParam as =
    -- JSON encode the individual params
    let ps :: [ParamValue] = fmap toParam as
     in toParam $ Array $ fromList $ fmap (String . (.value)) ps
instance {-# OVERLAPPABLE #-} (FromParam a) => FromParam [a] where
  parseParam p = do
    ts <- jsonParse @[Text] p
    mapM (parseParam . ParamValue) ts


instance (ToParam a) => ToParam (Maybe a) where
  toParam Nothing = ParamValue "~"
  toParam (Just a) = toParam a
instance {-# OVERLAPPABLE #-} (FromParam a) => FromParam (Maybe a) where
  parseParam (ParamValue "") = pure Nothing
  parseParam (ParamValue "~") = pure Nothing
  parseParam t = Just <$> parseParam @a t


  decodeFormValue Nothing = pure Nothing
  decodeFormValue (Just t) = do
    parseParam @(Maybe a) (ParamValue t)


instance {-# OVERLAPS #-} FromParam (Maybe Text) where
  parseParam (ParamValue "~") = pure Nothing
  -- keep empty strings, the default instance discards them
  parseParam (ParamValue "") = pure (Just "")
  parseParam t = Just <$> parseParam @Text t


  decodeFormValue Nothing = pure Nothing
  decodeFormValue (Just t) = do
    parseParam @(Maybe Text) (ParamValue t)


instance (ToParam a, ToParam b) => ToParam (Either a b) where
  toParam (Left a) = toParam a
  toParam (Right b) = toParam b
instance (FromParam a, FromParam b) => FromParam (Either a b) where
  parseParam val =
    case parseParam @a val of
      Right a -> pure $ Left a
      Left _ -> do
        case parseParam @b val of
          Left _ -> Left $ "Could not parse Either param: " <> show val
          Right b -> pure $ Right b


parseQueryParam :: (FromHttpApiData a) => ParamValue -> Either String a
parseQueryParam (ParamValue t) =
  first cs $ HttpApiData.parseQueryParam t


toQueryParam :: (ToHttpApiData a) => a -> ParamValue
toQueryParam a =
  ParamValue $ HttpApiData.toQueryParam a


-- | Encode a Show as a query param
showParam :: (Show a) => a -> ParamValue
showParam a = toQueryParam $ show a


-- | Decode a Read as a query param
readParam :: (Read a) => ParamValue -> Either String a
readParam p = do
  str <- parseQueryParam p
  case readMaybe str of
    Nothing -> Left $ cs $ "Could not read query param: " <> str
    Just a -> pure a


genericToParam :: (Generic a, GToJSON Zero (Rep a)) => a -> ParamValue
genericToParam a =
  case genericToJSON jsonOptions a of
    String t -> ParamValue t
    other -> jsonParam other


genericParseParam :: (Generic a, GFromJSON Zero (Rep a)) => ParamValue -> Either String a
genericParseParam (ParamValue t) = do
  val <- maybe (pure $ String t) pure $ A.decode (cs t)
  A.parseEither (genericParseJSON jsonOptions) val


-- Encoding ------------------------------------------------------------

jsonOptions :: A.Options
jsonOptions = defaultOptions{sumEncoding = TwoElemArray}


jsonParam :: (ToJSON a) => a -> ParamValue
jsonParam a = ParamValue (cs $ A.encode a)


jsonParse :: (FromJSON a) => ParamValue -> Either String a
jsonParse (ParamValue t) = do
  A.eitherDecode (cs t)
