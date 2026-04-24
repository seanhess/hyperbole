{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.Encoded where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Exception (Exception, catch, evaluate, throw)
import Data.Aeson (FromJSON (..), GFromJSON, GToJSON, Options (..), SumEncoding (..), ToJSON (..), Value (..), Zero, defaultOptions)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap (KeyMap, (!?))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Parser (eitherDecodeWith, json)
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString qualified as Atto
import Data.Attoparsec.ByteString.Char8 (isSpace, sepBy, takeWhile1)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.IsList (fromList, toList)


newtype ConName = ConName {text :: Text}
  deriving newtype (Eq, Show, IsString, Ord)
instance Semigroup ConName where
  -- Ignore the second constructor name
  c1 <> _ = c1
instance Monoid ConName where
  mempty = ConName ""


{- | Human Readable top-level encoding for ViewAction and ViewId
For simple Sum and Product types it is equivalent to the Show/Read instance

MyConstructor 1 2 3
OtherConstructor "hello" 2
-}
data Encoded = Encoded ConName [Argument]
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


data Argument
  = JSON Value
  | Hole
  deriving (Show, Eq, Ord, Generic, NFData)


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
  T.intercalate " " (con.text : fmap encodeArgument values)


-- | JSON Encode argument, but alter it slightly to use raw strings for constructors without parameters, and switch to two-element sum encoding
encodeArgument :: Argument -> Text
encodeArgument = \case
  (JSON (String s)) ->
    if isConstructorName s
      then s
      else encodeValue $ String s
  (JSON (Object km)) ->
    simplifySumEncoding $ Object km
  (JSON v) -> encodeValue v
  Hole -> "_"
 where
  encodeValue :: Value -> Text
  encodeValue = cs . A.encode

  isConstructorName :: Text -> Bool
  isConstructorName "" = False
  isConstructorName t =
    isUpper (T.head t) && T.all isAlphaNum t

  simplifySumEncoding :: Value -> Text
  simplifySumEncoding (Object o) = fromMaybe (encodeValue $ Object o) $ do
    innerProduct o <|> simpleTag o
  simplifySumEncoding v = encodeValue v

  innerProduct :: KeyMap Value -> Maybe Text
  innerProduct o = do
    String t <- o !? "tag"
    c <- o !? "contents"
    let contents = case c of
          Array a -> T.intercalate " " $ toList $ fmap encodeValue a
          other -> encodeValue other
    pure $ "(" <> t <> " " <> contents <> ")"

  simpleTag :: KeyMap Value -> Maybe Text
  simpleTag o = do
    t <- o !? "tag"
    case t of
      String s -> pure $ "(" <> s <> ")"
      v -> pure $ encodeValue v


encodedParseText :: Text -> Either String Encoded
encodedParseText inp =
  first cs $ AB.parseOnly encodedParser (cs inp)
 where
  encodedParser :: AB.Parser Encoded
  encodedParser = do
    con <- AC.takeTill AC.isSpace
    AC.skipSpace
    ps <- argumentParser `sepBy` AC.char ' '
    pure $ Encoded (ConName (cs con)) ps


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
instance ToEncoded () where
  toEncoded _ = mempty
instance ToEncoded Argument where
  toEncoded a = Encoded mempty [a]
instance ToEncoded Int where
  toEncoded n = toEncoded $ JSON (Number $ fromIntegral n)
instance ToEncoded Text where
  toEncoded t = toEncoded $ JSON (String t)


-- | Custom Encoding for embedding into web documents. Noteably used for 'ViewId' and 'ViewAction'
class FromEncoded a where
  parseEncoded :: Encoded -> Either String a
  default parseEncoded :: (Generic a, GFromEncoded (Rep a)) => Encoded -> Either String a
  parseEncoded = genericParseEncoded


instance FromEncoded Encoded where
  parseEncoded = pure
instance FromEncoded () where
  parseEncoded _ = pure ()
instance FromEncoded Argument where
  parseEncoded (Encoded _ as) = do
    case as of
      [a] -> pure a
      _ -> Left $ "Expected single argument value [arg] but got: " <> show as
instance FromEncoded Int where
  parseEncoded enc = do
    arg <- parseEncoded @Argument enc
    case arg of
      JSON (Number a) -> pure $ round a
      other -> Left $ "Expected Int but got " <> show other
instance FromEncoded Text where
  parseEncoded enc = do
    arg <- parseEncoded @Argument enc
    case arg of
      JSON (String t) -> pure t
      other -> Left $ "Expected Text but got " <> show other


fromResult :: A.Result a -> Either String a
fromResult (A.Success a) = pure a
fromResult (A.Error e) = Left (cs e)


-------------------------------------------------------------------------------
-- ARGUMENT ENCODING
-------------------------------------------------------------------------------
type ToArgument a = (ToJSON a)
type FromArgument a = (FromJSON a)


-- newtype ParsedArgument a = ParsedArgument a
--
--
-- instance (FromArgument a) => FromJSON (ParsedArgument a) where
--   parseJSON v = ParsedArgument <$> A.genericParseJSON argumentOptions v

-- TODO: we want to control the JSON serialization. Right now it serializes as "[]" for unary constructors, and {contents, tag} for others
-- TODO: override with specific tags
--

-- data Tag = A | B | C | D deriving (Generic, ToJSON) - currently "A"

argumentOptions :: A.Options
argumentOptions = defaultOptions{sumEncoding = TwoElemArray}


toArgument :: (ToArgument a) => a -> Argument
toArgument = JSON . toJSON


parseArgument :: forall a. (FromArgument a) => Argument -> Either String a
parseArgument (JSON v) = do
  case A.fromJSON v of
    A.Success a -> pure a
    A.Error e -> Left e
parseArgument Hole = Left "Cannot parse Argument Hole"


argumentParser :: Atto.Parser Argument
argumentParser =
  JSON <$> (json <|> constructor <|> innerProduct)
 where
  innerProduct = do
    _ <- AC.char '('
    tag <- AC.takeWhile1 isAlphaNum
    contents :: KeyMap Value <- innerContents <|> pure []
    _ <- AC.char ')'
    pure $ Object $ [("tag", String $ cs tag)] <> contents

  innerContents = do
    _ <- AC.char ' '
    args <- json `sepBy` AC.space
    pure $ case args of
      [] -> []
      [a] -> [("contents", a)]
      as -> [("contents", Array $ fromList as)]

  constructor = do
    s <- AC.takeWhile1 isAlphaNum
    pure $ String (cs s)


-- Param encoding scheme:
--   Wire format must not contain bare spaces (field separator) or real newlines.
--   We use backslash as the escape character:
--     '\'  → "\\"    (escape backslash itself, so it cannot be confused with an escape prefix)
--     '\n' → "\n"    (literal backslash + 'n', for real newline characters)
--     '_'  → "\_"    (escape underscore, since bare underscore encodes space)
--     ' '  → "_"     (encode space as underscore)
--   Decoding is the single-pass reverse of the above.
--
-- The critical invariant: backslash is escaped FIRST on encode and unescaped
-- LAST on decode.  This ensures that JSON escape sequences (e.g. the two chars
-- '\' 'n' inside "[\"\n\"]") are treated as an escaped backslash followed by
-- a plain 'n', not as the param newline escape.
-- See: https://github.com/seanhess/hyperbole/issues/187

-- desanitizeParamText :: Text -> Text
-- desanitizeParamText = go
--  where
--   go t = case T.uncons t of
--     Nothing -> ""
--     Just ('\\', rest) -> case T.uncons rest of
--       Just ('\\', rest') -> T.cons '\\' (go rest') -- \\ → \
--       Just ('n', rest') -> T.cons '\n' (go rest') -- \n → newline
--       Just ('_', rest') -> T.cons '_' (go rest') -- \_ → _
--       _ -> T.cons '\\' (go rest) -- bare backslash (shouldn't occur)
--     Just ('_', rest) -> T.cons ' ' (go rest) -- _ → space
--     Just (c, rest) -> T.cons c (go rest) -- other chars verbatim

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


instance (ToArgument a) => GToEncoded (K1 R a) where
  gToEncoded (K1 a) = do
    Encoded mempty [toArgumentWithHole a]


-- GFromEncoded: Generic ViewAction Decoding

class GFromEncoded f where
  gParseEncoded :: Encoded -> Either String (f p, [Argument])


instance (GFromEncoded f, GFromEncoded g) => GFromEncoded (f :+: g) where
  gParseEncoded enc@(Encoded con vals) = do
    let el = gParseEncoded @f enc
    let er = gParseEncoded @g enc
    case (el, er) of
      (Right (l, lvals), _) -> pure (L1 l, lvals)
      (_, Right (r, rvals)) -> pure (R1 r, rvals)
      (Left _e1, Left _e2) ->
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


instance (FromArgument a) => GFromEncoded (K1 R a) where
  gParseEncoded (Encoded con vals) = do
    case vals of
      (param : rest) -> do
        case parseArgument param of
          -- consume one param
          Right a -> pure (K1 a, rest)
          Left e -> Left (cs e)
      [] -> Left $ "Missing parameters for Encoded Constructor:" <> cs con.text


-- Input Holes -----------------------------------------------
-- Serializing a function is impossible. To simulate it, we need to fully apply the constructor with a "hole" for any expected inputs
-- This hole needs to be any type, so we pretend to have one with `throw`
-- later, when we convert a type to ParamValue, we catch it and serialize a placeholder

data InputHole = InputHole
  deriving (Show, Eq, Exception)


inputHole :: a
inputHole = throw InputHole


toArgumentWithHole :: (ToArgument a) => a -> Argument
toArgumentWithHole a = do
  unsafePerformIO $ do
    catch
      do
        a' <- evaluate a
        evaluate $ force $ toArgument a'
      do \InputHole -> pure Hole
