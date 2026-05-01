{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.Encoded where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString.Char8 (sepBy)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Bifunctor (first)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Web.Hyperbole.Data.Argument


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
  T.intercalate " " (con.text : fmap encodeFromArgument values)


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
  gToEncoded (K1 a) = do
    let JSON val = toArgument a
    Encoded mempty [JSON val]


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


instance (FromJSON a) => GFromEncoded (K1 R a) where
  gParseEncoded (Encoded con vals) = do
    case vals of
      (param : rest) -> do
        case parseArgument param of
          -- consume one param
          Right a -> pure (K1 a, rest)
          Left e -> Left (cs e)
      [] -> Left $ "Missing parameters for Encoded Constructor:" <> cs con.text


