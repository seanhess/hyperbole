{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Hyperbole.Data.Argument where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Exception (Exception, catch, evaluate, throw)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap (KeyMap, (!?))
import Data.Aeson.Parser (json)
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString qualified as Atto
import Data.Attoparsec.ByteString.Char8 (sepBy)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsList (..))
import GHC.Generics
import GHC.IO.Unsafe (unsafePerformIO)


data Argument
  = JSON Value
  deriving (Show, Eq, Ord, Generic, NFData)


-------------------------------------------------------------------------------
-- ARGUMENT ENCODING
-------------------------------------------------------------------------------

toArgument :: (ToJSON a) => a -> Argument
toArgument =
  JSON . toJSONCatchHole


parseArgument :: forall a. (FromJSON a) => Argument -> Either String a
parseArgument (JSON v) = do
  case A.fromJSON v of
    A.Success a -> pure a
    A.Error e -> Left e


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


-- decode a single argument
decodeToArgument :: Text -> Either String Argument
decodeToArgument inp =
  first (\e -> "decodeArgument: " <> cs inp <> " failed with error: " <> e) $ AB.parseOnly argumentParser (cs inp)


decodeArgument :: (FromJSON a) => Text -> Either String a
decodeArgument inp = do
  decodeToArgument inp >>= parseArgument


encodeArgument :: (ToJSON a) => a -> Text
encodeArgument a = encodeFromArgument $ toArgument a


-- | JSON Encode argument, but alter it slightly to use raw strings for constructors without parameters, and switch to two-element sum encoding
encodeFromArgument :: Argument -> Text
encodeFromArgument (JSON v)
  | v == holeValue = "|>_<|"
  | otherwise = encodeJSON v


encodeJSON :: Value -> Text
encodeJSON = \case
  (String s) ->
    if isConstructorName s
      then s
      else encodeValue $ String s
  (Object km) ->
    simplifySumEncoding $ Object km
  v -> encodeValue v
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


class UserInput a where
  parseInput :: Text -> Either String a
  default parseInput :: (FromJSON a) => Text -> Either String a
  parseInput = decodeArgument


instance UserInput Text where
  parseInput = pure


instance {-# OVERLAPPABLE #-} (UserInput a) => UserInput (Maybe a) where
  parseInput "" = pure Nothing
  parseInput t = Just <$> parseInput @a t


instance {-# OVERLAPS #-} UserInput (Maybe Text) where
  parseInput = pure . Just


-- Input Holes -----------------------------------------------
-- Serializing a function is impossible. To simulate it, we need to fully apply the constructor with a "hole" for any expected inputs
-- This hole needs to be any type, so we pretend to have one with `throw`
-- later, when we convert a type to ParamValue, we catch it and serialize a placeholder

toJSONCatchHole :: (ToJSON a) => a -> Value
toJSONCatchHole a =
  unsafePerformIO $ do
    catch
      do toJSON <$> evaluate a
      do \ExpectedInput -> pure holeValue


data ExpectedInput = ExpectedInput
  deriving (Show, Eq, Exception)


expectInput :: a
expectInput = throw ExpectedInput


holeArg :: Argument
holeArg = JSON holeValue


holeValue :: Value
holeValue = String "|>_<|"
