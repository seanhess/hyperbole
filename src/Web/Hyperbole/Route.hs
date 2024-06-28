{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Route
  ( Route (..)
  , findRoute
  , pathUrl
  , routeUrl
  , GenRoute (..)
  , genRouteRead
  , Url
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text (Text, pack, toLower, unpack)
import Data.Text qualified as T
import GHC.Generics
import Text.Read (readMaybe)
import Web.View.Types.Url (Segment, Url, pathUrl)
import Prelude hiding (dropWhile)


{- | Derive this class to use a sum type as a route. Constructors and Selectors map intuitively to url patterns

> data AppRoute
>  = HomePage
>  | Users
>  | User Int
>  deriving (Generic, Route)
>
> /         -> HomePage
> /users/   -> Users
> /user/100 -> User 100
-}
class Route a where
  -- | The default route to use if attempting to match on empty segments
  defRoute :: a
  default defRoute :: (Generic a, GenRoute (Rep a)) => a
  defRoute = to genFirst


  -- | Try to match segments to a route
  matchRoute :: [Segment] -> Maybe a
  default matchRoute :: (Generic a, GenRoute (Rep a)) => [Segment] -> Maybe a
  -- this will match a trailing slash, but not if it is missing
  matchRoute [""] = pure defRoute
  matchRoute [] = pure defRoute
  matchRoute segs = to <$> genRoute segs


  -- | Map a route to segments
  routePath :: a -> [Segment]
  default routePath :: (Generic a, Eq a, GenRoute (Rep a)) => a -> [Segment]
  routePath p
    | p == defRoute = []
    | otherwise = filter (not . T.null) $ genPaths $ from p


-- | Try to match a route, use 'defRoute' if it's empty
findRoute :: (Route a) => [Segment] -> Maybe a
findRoute [] = Just defRoute
findRoute ps = matchRoute ps


{- | Convert a 'Route' to a 'Url'

>>> routeUrl (User 100)
/user/100
-}
routeUrl :: (Route a) => a -> Url
routeUrl = pathUrl . routePath


-- | Automatically derive 'Route'
class GenRoute f where
  genRoute :: [Text] -> Maybe (f p)
  genPaths :: f p -> [Text]
  genFirst :: f p


-- datatype metadata
instance (GenRoute f) => GenRoute (M1 D c f) where
  genRoute ps = M1 <$> genRoute ps
  genPaths (M1 x) = genPaths x
  genFirst = M1 genFirst


-- Constructor names / lines
instance (Constructor c, GenRoute f) => GenRoute (M1 C c f) where
  genRoute (n : ps) = do
    -- take the first path off the list
    -- check that it matches the constructor name
    -- check that the rest matches
    let name = conName (undefined :: M1 C c f x)
    guard (n == toLower (pack name))
    M1 <$> genRoute ps
  genRoute [] = Nothing


  genFirst = M1 genFirst


  genPaths (M1 x) =
    let name = conName (undefined :: M1 C c f x)
     in toLower (pack name) : genPaths x


-- Unary constructors
instance GenRoute U1 where
  genRoute [] = pure U1
  genRoute _ = Nothing
  genPaths _ = []
  genFirst = U1


-- Selectors
instance (GenRoute f) => GenRoute (M1 S c f) where
  genRoute ps =
    M1 <$> genRoute ps


  genFirst = M1 genFirst


  genPaths (M1 x) = genPaths x


-- Sum types
instance (GenRoute a, GenRoute b) => GenRoute (a :+: b) where
  genRoute ps = L1 <$> genRoute ps <|> R1 <$> genRoute ps
  genFirst = L1 genFirst
  genPaths (L1 a) = genPaths a
  genPaths (R1 a) = genPaths a


-- Product types
instance (GenRoute a, GenRoute b) => GenRoute (a :*: b) where
  genRoute (p : ps) = do
    ga <- genRoute [p]
    gr <- genRoute ps
    pure $ ga :*: gr
  genRoute _ = Nothing


  genFirst = genFirst :*: genFirst


  genPaths (a :*: b) = genPaths a <> genPaths b


instance (Route sub) => GenRoute (K1 R sub) where
  genRoute ts = K1 <$> matchRoute ts
  genFirst = K1 defRoute
  genPaths (K1 sub) = routePath sub


genRouteRead :: (Read x) => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (unpack t)
genRouteRead _ = Nothing


instance Route Text where
  matchRoute [t] = pure t
  matchRoute _ = Nothing
  routePath t = [t]
  defRoute = ""


instance Route String where
  matchRoute [t] = pure (unpack t)
  matchRoute _ = Nothing
  routePath t = [pack t]
  defRoute = ""


instance Route Integer where
  matchRoute = matchRouteRead
  routePath = routePathShow
  defRoute = 0


instance Route Int where
  matchRoute = matchRouteRead
  routePath = routePathShow
  defRoute = 0


instance (Route a) => Route (Maybe a) where
  matchRoute [] = pure Nothing
  matchRoute ps = Just <$> matchRoute ps
  routePath (Just a) = routePath a
  routePath Nothing = []
  defRoute = Nothing


matchRouteRead :: (Read a) => [Segment] -> Maybe a
matchRouteRead [t] = readMaybe (unpack t)
matchRouteRead _ = Nothing


routePathShow :: (Show a) => a -> [Segment]
routePathShow a = [pack (show a)]
