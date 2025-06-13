{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.Data.Cookie where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (urlDecode, urlEncode)
import Web.View.Types.Url (Segment, pathUrl, renderUrl)



type Key  = Text

data Cookie = Cookie
  { key :: Key
  , path :: Maybe [Segment]
  , value :: Maybe CookieValue
  }
  deriving (Show, Eq)


newtype Cookies = Cookies (Map Key Cookie)
  deriving newtype (Monoid, Semigroup, Show, Eq)


newtype CookieValue = CookieValue ByteString
  deriving newtype (Show, Eq)


insert :: Cookie -> Cookies -> Cookies
insert cookie (Cookies m) =
  Cookies $ M.insert cookie.key cookie m


delete :: Key -> Cookies -> Cookies
delete key (Cookies m) =
  Cookies $ M.delete key m


lookup :: Key -> Cookies -> Maybe CookieValue
lookup key (Cookies m) = do
  cook <- M.lookup key m
  cook.value





fromList :: [Cookie] -> Cookies
fromList cks = Cookies $ M.fromList (fmap keyValue cks)
 where
  keyValue c = (c.key, c)


toList :: Cookies -> [Cookie]
toList (Cookies m) = M.elems m




render :: [Segment] -> Cookie -> ByteString
render requestPath cookie =
  let path = fromMaybe requestPath cookie.path
   in cs cookie.key <> "=" <> value cookie.value <> "; SameSite=None; secure; path=" <> cs (renderUrl (pathUrl path))
 where
  value Nothing = "; expires=Thu, 01 Jan 1970 00:00:00 GMT"
  value (Just (CookieValue val)) = urlEncode True $ cs val


parse :: [(ByteString, ByteString)] -> Either Text Cookies
parse kvs = do
  cks <- mapM (uncurry parseValue) kvs
  pure $ fromList cks


parseValue :: ByteString -> ByteString -> Either Text Cookie
parseValue k val = do
  let cval = CookieValue $ cs $ urlDecode True val
  pure $ Cookie (cs k) Nothing (Just $ cval)
