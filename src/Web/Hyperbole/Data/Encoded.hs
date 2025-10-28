{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Hyperbole.Data.Encoded where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString qualified as Atto
import Data.Attoparsec.ByteString.Char8 (isSpace, sepBy, takeWhile1)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Bifunctor (first)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Web.Hyperbole.Data.Param


newtype ConName = ConName {text :: Text}
  deriving newtype (Eq, Show, IsString, Ord)
instance Semigroup ConName where
  -- Ignore the second constructor name
  c1 <> _ = c1
instance Monoid ConName where
  mempty = ConName ""


{- | Pretty Human Readable top-levelencoding for ViewAction and ViewId
For simple Sum and Product types it is equivalent to the Show/Read instance

MyConstructor 1 2 3
-}
data Encoded = Encoded ConName [ParamValue]
  deriving (Show, Eq, Ord)


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


decodeEither :: (FromEncoded a) => Text -> Either String a
decodeEither t = do
  enc <- encodedParseText t
  parseEncoded enc


-- | Basic Encoding
encodedToText :: Encoded -> Text
encodedToText (Encoded con values) =
  let params = T.intercalate " " $ fmap encodeParam values
   in case params of
        "" -> con.text
        _ -> con.text <> " " <> params


encodedParseText :: Text -> Either String Encoded
encodedParseText inp =
  first cs $ AB.parseOnly encodedParser (cs inp)
 where
  encodedParser :: AB.Parser Encoded
  encodedParser = do
    con <- AC.takeTill AC.isSpace
    AC.skipSpace
    params <- paramParser `sepBy` AC.char ' '
    pure $ Encoded (ConName (cs con)) params


genericToEncoded :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
genericToEncoded a = gToEncoded (from a)


genericParseEncoded :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either String a
genericParseEncoded enc = do
  (gen, _) <- gParseEncoded enc
  pure $ to gen


genericDecode :: (Generic a, GFromEncoded (Rep a)) => Text -> Maybe a
genericDecode t = either (const Nothing) Just $ do
  enc <- encodedParseText t
  genericParseEncoded enc


-- | Custom Encoding for embedding into web documents. Noteably used for 'ViewId' and 'ViewAction'
class ToEncoded a where
  toEncoded :: a -> Encoded
  default toEncoded :: (Generic a, GToEncoded (Rep a)) => a -> Encoded
  toEncoded = genericToEncoded


instance ToEncoded Encoded where
  toEncoded = id


-- | Custom Encoding for embedding into web documents. Noteably used for 'ViewId' and 'ViewAction'
class FromEncoded a where
  parseEncoded :: Encoded -> Either String a
  default parseEncoded :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either String a
  parseEncoded = genericParseEncoded


instance FromEncoded Encoded where
  parseEncoded = pure


fromResult :: A.Result a -> Either String a
fromResult (A.Success a) = pure a
fromResult (A.Error e) = Left (cs e)


-------------------------------------------------------------------------------
-- PARAM ENCODING
-------------------------------------------------------------------------------
-- Params need to be sanitized and escaped, because we want to use spaces to separate our params
-- Data.Param by default does not sanitize spaces

paramParser :: Atto.Parser ParamValue
paramParser = do
  t <- takeWhile1 (not . isSpace)
  pure $ decodeParam $ cs t


decodeParam :: Text -> ParamValue
decodeParam = \case
  "|" -> ParamValue ""
  t -> ParamValue $ desanitizeParamText t


-- replace all underscores that are NOT "\\_" with spaces

desanitizeParamText :: Text -> Text
desanitizeParamText =
  T.replace "\\ " "_" . T.replace "_" " " . T.replace "\\n" "\n"


--   | T.isSuffixOf "\\" seg = T.dropEnd 1 seg <> "_" <> txt
--   | otherwise = seg <> " " <> txt

-- foldr join "" $
-- where
--
-- join "" "" = " "
-- join "" " " = " "
-- join seg "" = seg
-- join seg txt
--   | T.isSuffixOf "\\" seg = T.dropEnd 1 seg <> "_" <> txt
--   | otherwise = seg <> " " <> txt

encodeParam :: ParamValue -> Text
encodeParam (ParamValue t) =
  case t of
    "" -> "|"
    _ -> sanitizeParamText t
 where
  -- Q: Should we also sanitize \r\n?
  sanitizeParamText :: Text -> Text
  sanitizeParamText =
    T.replace " " "_" . T.replace "_" "\\_" . T.replace "\n" "\\n"


-- decodeParamValue :: (FromParam a) => Text -> Either String a
-- decodeParamValue = parseParam . decodeParam

-- decodeParam :: Text -> ParamValue
-- decodeParam inp = do
--   case A.eitherDecode (cs inp) of
--     Left _ -> paramFromText inp
--     Right v -> ParamValue inp v

-------------------------------------------------------------------------------
-- GENERICS
-------------------------------------------------------------------------------

-- GToEncoded: Generic ViewAction Encoding

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


instance (ToParam a) => GToEncoded (K1 R a) where
  gToEncoded (K1 a) = Encoded mempty [toParam a]


-- GFromEncoded: Generic ViewAction Decoding

class GFromEncoded f where
  gParseEncoded :: Encoded -> Either String (f p, [ParamValue])


instance (GFromEncoded f, GFromEncoded g) => GFromEncoded (f :+: g) where
  gParseEncoded enc@(Encoded con vals) = do
    let el = gParseEncoded @f enc
    let er = gParseEncoded @g enc
    case (el, er) of
      (Right (l, lvals), _) -> pure (L1 l, lvals)
      (_, Right (r, rvals)) -> pure (R1 r, rvals)
      (Left _, Left _) ->
        Left $ "No matching sum constructor: " <> cs con.text <> " " <> cs (show vals)


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
    if cs cname.text == con
      then first M1 <$> gParseEncoded @f enc
      else Left $ "Mismatched Constructor " <> cs cname.text <> " /= " <> con
   where
    con = cs $ conName (undefined :: M1 C c f p)


instance (GFromEncoded f) => GFromEncoded (M1 S s f) where
  gParseEncoded enc = do
    (a, rest) <- gParseEncoded enc
    pure (M1 a, rest)


instance (FromParam a) => GFromEncoded (K1 R a) where
  gParseEncoded (Encoded con vals) = do
    case vals of
      (param : rest) -> do
        case parseParam param of
          -- consume one param
          Right a -> pure (K1 a, rest)
          Left e -> Left (cs e)
      [] -> Left $ "Missing parameters for Encoded Constructor:" <> cs con.text
