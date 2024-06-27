{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Param where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import GHC.Generics
import Text.Read (readMaybe)
import Web.HttpApiData


{- | Types that can be serialized. 'HyperView' requires this for both its view id and action

> data Message = Message Int
>   deriving (Generic, Param)
-}
class Param a where
  toParam :: a -> Text
  default toParam :: (Generic a, GParam (Rep a)) => a -> Text
  toParam = gToParam . from


  -- not as flexible as FromHttpApiData, but derivable
  parseParam :: Text -> Maybe a
  default parseParam :: (Generic a, GParam (Rep a)) => Text -> Maybe a
  parseParam t = to <$> gParseParam t


class GParam f where
  gToParam :: f p -> Text
  gParseParam :: Text -> Maybe (f p)


instance (GParam f, GParam g) => GParam (f :*: g) where
  gToParam (a :*: b) = gToParam a <> "-" <> gToParam b
  gParseParam t = do
    let (at, bt) = breakSegment '-' t
    a <- gParseParam at
    b <- gParseParam bt
    pure $ a :*: b


instance (GParam f, GParam g) => GParam (f :+: g) where
  gToParam (L1 a) = gToParam a
  gToParam (R1 b) = gToParam b
  gParseParam t = do
    (L1 <$> gParseParam @f t) <|> (R1 <$> gParseParam @g t)


-- do we add the datatypename? no, the constructor name
instance (Datatype d, GParam f) => GParam (M1 D d f) where
  gToParam (M1 a) = gToParam a
  gParseParam t = M1 <$> gParseParam t


instance (Constructor c, GParam f) => GParam (M1 C c f) where
  gToParam (M1 a) =
    let cn = toSegment (conName (undefined :: M1 C c f p))
     in case gToParam a of
          "" -> cn
          t -> cn <> "-" <> t
  gParseParam t = do
    let (c, rest) = breakSegment '-' t
    guard $ c == toSegment (conName (undefined :: M1 C c f p))
    M1 <$> gParseParam rest


instance GParam U1 where
  gToParam _ = ""
  gParseParam _ = pure U1


instance (GParam f) => GParam (M1 S s f) where
  gToParam (M1 a) = gToParam a
  gParseParam t = M1 <$> gParseParam t


instance {-# OVERLAPPABLE #-} (Param a) => GParam (K1 R a) where
  gToParam (K1 a) = toParam a
  gParseParam t = K1 <$> parseParam t


-- instance {-# OVERLAPPABLE #-} (Show a, Read a) => GParam (K1 R a) where
--   gToParam (K1 a) = pack $ show a
--   gParseParam t = do
--     K1 <$> readMaybe (unpack t)

breakSegment :: Char -> Text -> (Text, Text)
breakSegment c t =
  let (start, rest) = T.breakOn (pack [c]) t
   in (start, T.drop 1 rest)


toSegment :: String -> Text
toSegment = T.toLower . pack


-- instance (GParam f) => GParam (M1 C c f) where
--   gForm = M1 gForm

-- where
--  toDouble '\'' = '\"'
--  toDouble c = c

instance (Param a) => Param (Maybe a) where
  toParam Nothing = ""
  toParam (Just a) = toParam a
  parseParam "" = pure Nothing
  parseParam t = Just $ parseParam t
instance Param Integer where
  toParam = toQueryParam
  parseParam = parseParamQuery
instance Param Float where
  toParam = toQueryParam
  parseParam = parseParamQuery
instance Param Int where
  toParam = toQueryParam
  parseParam = parseParamQuery
instance Param () where
  toParam = toQueryParam
  parseParam = parseParamQuery
instance Param Text where
  parseParam = parseParamQuery
  toParam = toQueryParam
instance Param String where
  parseParam = parseParamQuery
  toParam = toQueryParam


-- | Easily derive Param for a type that implements HttpApiData with this and toQueryParam
parseParamQuery :: (FromHttpApiData a) => Text -> Maybe a
parseParamQuery t =
  either (const Nothing) Just $ parseQueryParam t


class GAction f where
  gToAction :: f p -> Text
  gParseAction :: Text -> Maybe (f p)


instance (GAction f, GAction g) => GAction (f :*: g) where
  gToAction (a :*: b) = gToAction a <> " " <> gToAction b


  gParseAction t = do
    let (at, bt) = breakProduct
    a <- gParseAction at
    b <- gParseAction bt
    pure $ a :*: b
   where
    breakProduct
      -- if it starts with a ( or a ", we want to skip until the terminator
      | T.isPrefixOf "\"" t = breakString
      | T.isPrefixOf "(" t = breakParens
      | otherwise = breakSegment ' ' t

    breakString =
      let rest = T.drop 1 t
       in ("\"" <> T.takeWhile (/= '"') rest <> "\"", T.drop 2 $ T.dropWhile (/= '"') rest)

    breakParens =
      let rest = T.drop 1 t
       in ("(" <> T.takeWhile (/= ')') rest <> ")", T.drop 2 $ T.dropWhile (/= ')') rest)


instance (GAction f, GAction g) => GAction (f :+: g) where
  gToAction (L1 a) = gToAction a
  gToAction (R1 b) = gToAction b
  gParseAction t = do
    (L1 <$> gParseAction @f t) <|> (R1 <$> gParseAction @g t)


instance (Datatype d, GAction f) => GAction (M1 D d f) where
  gToAction (M1 a) = gToAction a
  gParseAction t = M1 <$> gParseAction t


instance (Constructor c, GAction f) => GAction (M1 C c f) where
  gToAction (M1 a) =
    let cn = conName (undefined :: M1 C c f p)
     in case gToAction a of
          "" -> pack cn
          t -> pack cn <> " " <> t
  gParseAction t = do
    let (c, rest) = breakSegment ' ' t
    guard $ c == pack (conName (undefined :: M1 C c f p))
    M1 <$> gParseAction rest


instance GAction U1 where
  gToAction _ = ""
  gParseAction _ = pure U1


instance (GAction f) => GAction (M1 S s f) where
  gToAction (M1 a) = gToAction a
  gParseAction t = M1 <$> gParseAction t


instance GAction (K1 R Int) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance GAction (K1 R Integer) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance GAction (K1 R String) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance GAction (K1 R Text) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance GAction (K1 R BS.ByteString) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance GAction (K1 R BL.ByteString) where
  gToAction = toActionShow
  gParseAction = parseActionRead


instance {-# OVERLAPPABLE #-} (Show a, Read a) => GAction (K1 R a) where
  gToAction (K1 a) = "(" <> pack (show a) <> ")"
  gParseAction t = K1 <$> readMaybe (unpack t)


toActionShow :: (Show a) => K1 R a p -> Text
toActionShow (K1 a) = pack (show a)


parseActionRead :: (Read a) => Text -> Maybe (K1 R a p)
parseActionRead t = K1 <$> readMaybe (unpack t)
