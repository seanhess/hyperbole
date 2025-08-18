{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Hyperbole.Data.Encoded where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson qualified as A
import Data.Aeson.Parser (json)
import Data.Attoparsec.ByteString qualified as Atto
import Data.Attoparsec.ByteString.Char8 (char, isSpace, sepBy, skipSpace, takeTill)
import Data.Bifunctor (first)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics


newtype ConName = ConName {text :: Text}
  deriving newtype (Eq, Show, IsString)
instance Semigroup ConName where
  -- Ignore the second constructor name
  c1 <> _ = c1
instance Monoid ConName where
  mempty = ConName ""


{- | Pretty Human Readable top-levelencoding for ViewAction and ViewId
For simple Sum and Product types it is equivalent to the Show/Read instance

MyConstructor 1 2 3
-}
data Encoded = Encoded ConName [Value]
  deriving (Show, Eq)


instance Semigroup Encoded where
  Encoded c1 es1 <> Encoded c2 es2 =
    Encoded (c1 <> c2) (es1 <> es2)
instance Monoid Encoded where
  mempty = Encoded mempty mempty
instance ToJSON Encoded where
  toJSON e = toJSON $ encode e
instance FromJSON Encoded where
  parseJSON (String t) =
    case decodeEither t of
      Left e -> fail $ "Encoded " <> cs e
      Right a -> pure a
  parseJSON val = fail $ "Expected Encoded but got: " <> show val


encode :: (ToEncoded a) => a -> Text
encode a = encodedToText $ toEncoded a


decode :: (FromEncoded a) => Text -> Maybe a
decode t = either (const Nothing) Just $ decodeEither t


decodeEither :: (FromEncoded a) => Text -> Either Text a
decodeEither t = do
  enc <- encodedParseText t
  parseEncoded enc


-- | Basic Encoding
encodedToText :: Encoded -> Text
encodedToText (Encoded con values) =
  let params = T.intercalate " " $ fmap (cs . A.encode) values
   in case params of
        "" -> con.text
        _ -> con.text <> " " <> params


encodedParseText :: Text -> Either Text Encoded
encodedParseText inp =
  first cs $ Atto.parseOnly encodedParser (cs inp)
 where
  encodedParser :: Atto.Parser Encoded
  encodedParser = do
    con <- takeTill isSpace
    skipSpace
    params <- json `sepBy` char ' '
    pure $ Encoded (ConName (cs con)) params


genericToEncoded :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
genericToEncoded a = gToEncoded (from a)


genericParseEncoded :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either Text a
genericParseEncoded enc = do
  (gen, _) <- gParseEncoded enc
  pure $ to gen


genericDecode :: (Generic a, GFromEncoded (Rep a)) => Text -> Maybe a
genericDecode t = either (const Nothing) Just $ do
  enc <- encodedParseText t
  genericParseEncoded enc


class ToEncoded a where
  toEncoded :: a -> Encoded
  default toEncoded :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toEncoded = genericToEncoded


instance ToEncoded Encoded where
  toEncoded = id


class FromEncoded a where
  parseEncoded :: Encoded -> Either Text a
  default parseEncoded :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either Text a
  parseEncoded = genericParseEncoded


instance FromEncoded Encoded where
  parseEncoded = pure


fromResult :: A.Result a -> Either Text a
fromResult (A.Success a) = pure a
fromResult (A.Error e) = Left (cs e)


-- JSON Encoded Data ------------------------------------

{- | This type always encodes data via JSON
newtype JSON a = JSON a


instance (ToJSON a) => ToParam (JSON a) where
  toParam (JSON a) = ParamValue $ cs (A.encode a)
instance (FromJSON a) => FromParam (JSON a) where
  parseParam (ParamValue t) = do
    case A.eitherDecode (cs t) of
      Left e -> Left (cs e)
      Right a -> pure $ JSON a


instance (ToJSON a) => ToEncoded (JSON a) where
  toEncoded (JSON a) = toEncoded $ toJSON a
instance (FromJSON a) => FromEncoded (JSON a) where
  parseEncoded enc = do
    val <- parseEncoded @A.Value enc
    bimap id JSON <$> fromResult $ fromJSON val
-}

-- Read/Show Encoded Data ------------------------------------

-- newtype ShowRead a = ShowRead a
--
--
-- instance (Show a) => ToParam (ShowRead a) where
--   toParam (ShowRead a) = ParamValue $ cs (show a)
-- instance (Read a) => FromParam (ShowRead a) where
--   parseParam (ParamValue t) = do
--     case readMaybe (cs t) of
--       Nothing -> Left $ "Could not read " <> t
--       Just a -> pure $ ShowRead a

-- -- GToEncoded: Generic ViewAction Encoding -----------------------------------------

class GToEncoded f where
  gToEncoded :: f p -> Encoded


instance (GToEncoded f, GToEncoded g) => GToEncoded (f :+: g) where
  gToEncoded (L1 f) = gToEncoded f
  gToEncoded (R1 f) = gToEncoded f


instance (GToEncoded f, GToEncoded g) => GToEncoded (f :*: g) where
  gToEncoded (f :*: g) =
    gToEncoded f <> gToEncoded g


instance GToEncoded U1 where
  -- WARNING: not sure if this will work
  gToEncoded U1 = mempty


instance (GToEncoded f) => GToEncoded (M1 D d f) where
  gToEncoded (M1 f) = gToEncoded f


instance (Constructor c, GToEncoded f) => GToEncoded (M1 C c f) where
  gToEncoded (M1 f) =
    let con = cs (conName (undefined :: M1 C c f p))
     in Encoded (ConName con) mempty <> gToEncoded f


instance (GToEncoded f) => GToEncoded (M1 S s f) where
  gToEncoded (M1 f) = gToEncoded f


instance (ToJSON a) => GToEncoded (K1 R a) where
  gToEncoded (K1 a) = Encoded mempty [toJSON a]


-- GFromEncoded: Generic ViewAction Decoding -----------------------------------------

class GFromEncoded f where
  gParseEncoded :: Encoded -> Either Text (f p, [Value])


instance (GFromEncoded f, GFromEncoded g) => GFromEncoded (f :+: g) where
  gParseEncoded enc@(Encoded con vals) = do
    let el = gParseEncoded @f enc
    let er = gParseEncoded @g enc
    case (el, er) of
      (Right (l, lvals), _) -> pure (L1 l, lvals)
      (_, Right (r, rvals)) -> pure (R1 r, rvals)
      (Left _, Left _) ->
        Left $ "No matching sum constructor: " <> con.text <> " " <> cs (show vals)


instance (GFromEncoded f, GFromEncoded g) => GFromEncoded (f :*: g) where
  gParseEncoded (Encoded con vals) = do
    (a, rest) <- gParseEncoded @f (Encoded con vals)
    (b, gone) <- gParseEncoded @g (Encoded con rest)
    pure (a :*: b, gone)


instance GFromEncoded U1 where
  gParseEncoded (Encoded _ vals) = pure (U1, vals)


instance (GFromEncoded f) => GFromEncoded (M1 D d f) where
  gParseEncoded enc = do
    first M1 <$> gParseEncoded enc


instance (Constructor c, GFromEncoded f) => GFromEncoded (M1 C c f) where
  gParseEncoded enc@(Encoded cname _) = do
    if cname.text == con
      then first M1 <$> gParseEncoded @f enc
      else Left $ "Mismatched Constructor " <> cname.text <> " /= " <> con
   where
    con = cs $ conName (undefined :: M1 C c f p)


instance (GFromEncoded f) => GFromEncoded (M1 S s f) where
  gParseEncoded enc = do
    (a, rest) <- gParseEncoded enc
    pure (M1 a, rest)


instance (FromJSON a) => GFromEncoded (K1 R a) where
  gParseEncoded (Encoded con vals) = do
    case vals of
      (param : rest) -> do
        case A.fromJSON param of
          -- consume one param
          A.Success a -> pure (K1 a, rest)
          A.Error e -> Left (cs e)
      [] -> Left $ "Missing parameters for Encoded Constructor:" <> con.text
