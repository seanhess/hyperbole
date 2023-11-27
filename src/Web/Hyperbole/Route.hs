{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Route where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.String (IsString (..))
import Data.Text (Text, dropWhile, dropWhileEnd, intercalate, pack, splitOn, toLower, unpack)
import GHC.Generics
import Text.Read (readMaybe)
import Web.View.Types (Url (..))
import Prelude hiding (dropWhile)


type IsAbsolute = Bool
type Segment = Text
data Path = Path
  { isAbsolute :: Bool
  , segments :: [Segment]
  }
  deriving (Show)


-- what if you want a relative url?
instance IsString Path where
  fromString s = Path (isRoot s) [cleanSegment $ pack s]
   where
    isRoot ('/' : _) = True
    isRoot _ = False


class Route a where
  matchRoute :: Path -> Maybe a
  routePath :: a -> Path
  defRoute :: a


  default matchRoute :: (Generic a, GenRoute (Rep a)) => Path -> Maybe a
  -- this will match a trailing slash, but not if it is missing
  matchRoute (Path _ [""]) = pure defRoute
  matchRoute (Path _ segs) = to <$> genRoute segs


  default routePath :: (Generic a, Eq a, GenRoute (Rep a)) => a -> Path
  routePath p
    | p == defRoute = Path True [""]
    | otherwise = Path True $ genPaths $ from p


  default defRoute :: (Generic a, GenRoute (Rep a)) => a
  defRoute = to genFirst


pathUrl :: Path -> Url
pathUrl (Path True ss) = Url $ "/" <> intercalate "/" ss
pathUrl (Path False ss) = Url $ intercalate "/" ss


cleanSegment :: Segment -> Segment
cleanSegment = dropWhileEnd (== '/') . dropWhile (== '/')


pathSegments :: Text -> [Segment]
pathSegments path = splitOn "/" $ dropWhile (== '/') path


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
  genRoute ts = K1 <$> matchRoute (Path True ts)
  genFirst = K1 defRoute
  genPaths (K1 sub) = (routePath sub).segments


genRouteRead :: (Read x) => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (unpack t)
genRouteRead _ = Nothing


instance Route Text where
  matchRoute (Path _ [t]) = pure t
  matchRoute _ = Nothing
  routePath t = Path False [t]
  defRoute = ""


instance Route String where
  matchRoute (Path _ [t]) = pure (unpack t)
  matchRoute _ = Nothing
  routePath t = Path False [pack t]
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
  matchRoute (Path _ []) = pure Nothing
  matchRoute ps = Just <$> matchRoute ps
  routePath (Just a) = routePath a
  routePath Nothing = Path False []
  defRoute = Nothing


matchRouteRead :: (Read a) => Path -> Maybe a
matchRouteRead (Path _ [t]) = readMaybe (unpack t)
matchRouteRead _ = Nothing


routePathShow :: (Show a) => a -> Path
routePathShow a = Path False [pack (show a)]
