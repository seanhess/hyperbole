{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Data.Session where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics
import Web.Hyperbole.Data.QueryData (Param (..), ParamValue (..), ToParam (..))
import Web.View.Types.Url (Segment)


data Cookie = Cookie
  { key :: Param
  , value :: Maybe ParamValue
  , path :: Maybe [Segment]
  }
  deriving (Show, Eq)
newtype Cookies = Cookies (Map Param Cookie)
  deriving newtype (Monoid, Semigroup, Show)


insert :: Cookie -> Cookies -> Cookies
insert cookie (Cookies m) =
  Cookies $ M.insert cookie.key cookie m


delete :: Param -> Cookies -> Cookies
delete key (Cookies m) =
  Cookies $ M.delete key m


lookup :: Param -> Cookies -> Maybe ParamValue
lookup key (Cookies m) = do
  cook <- M.lookup key m
  cook.value


deletedCookie :: forall a. (Session a) => Cookie
deletedCookie =
  Cookie (sessionKey @a) Nothing (cookiePath @a)


sessionCookie :: forall a. (Session a, ToParam a) => a -> Cookie
sessionCookie a = Cookie (sessionKey @a) (Just $ toParam a) (cookiePath @a)


fromList :: [Cookie] -> Cookies
fromList cks = Cookies $ M.fromList (fmap keyValue cks)
 where
  keyValue c = (c.key, c)


toList :: Cookies -> [Cookie]
toList (Cookies m) = M.elems m


{- | Configure a data type to persist in the 'session'

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences
@
-}
class Session a where
  -- | Unique key for the Session. Defaults to the datatypeName
  sessionKey :: Param
  default sessionKey :: (Generic a, GDatatypeName (Rep a)) => Param
  sessionKey = Param $ gDatatypeName $ from (undefined :: a)


  -- | By default Sessions are persisted only to the current page. Set this to `Just []` to make an application-wide Session
  cookiePath :: Maybe [Segment]
  default cookiePath :: Maybe [Segment]
  cookiePath = Nothing


-- | generic datatype name
class GDatatypeName f where
  gDatatypeName :: f p -> Text


instance (Datatype d) => GDatatypeName (M1 D d f) where
  gDatatypeName _ =
    cs $ datatypeName (undefined :: M1 D d f p)
