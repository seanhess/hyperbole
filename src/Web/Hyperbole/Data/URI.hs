module Web.Hyperbole.Data.URI
  ( -- * URI
    URI (..)
  , URIAuth (..)
  , uri

    -- ** Path
  , Path (..)
  , Segment
  , path
  , parseURIReference
  , pathUri
  , uriToText
  , pathToText

    -- ** Query String
  , queryString
  , parseQuery
  , queryInsert
  , Query
  , QueryItem
  , (./.)
  , (.?.)
  , cleanSegment
  )
where

import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsList (..))
import Network.HTTP.Types (Query, QueryItem, parseQuery, renderQuery)
import Network.URI (URI (..), URIAuth (..), parseURIReference, uriToString)
import Network.URI qualified as Network
import Network.URI.Static (uri)
import System.FilePath ((</>))


-- Constructors ------------------------------------------
-- see `uri` for static URIs

-- Operators -----------------------------------------------

(./.) :: URI -> Path -> URI
u ./. p =
  let newPath = cs $ pathToText p
   in u{Network.uriPath = if p.isRoot then newPath else u.uriPath </> newPath}
infixl 5 ./.


(.?.) :: URI -> QueryItem -> URI
u .?. (k, mv) = u{uriQuery = queryInsert k mv u.uriQuery}


-- Query ---------------------------------------------------

type QueryString = String


queryInsert :: ByteString -> Maybe ByteString -> QueryString -> QueryString
queryInsert k mv s =
  queryString $ parseQuery (cs s) <> [(k, mv)]


queryString :: [(ByteString, Maybe ByteString)] -> QueryString
queryString = cs . renderQuery True


-- Path -----------------------------------------------------

data Path = Path
  { isRoot :: Bool
  , segments :: [Segment]
  }
  deriving (Show, Eq)
instance IsList Path where
  type Item Path = Segment
  fromList = Path True
  toList p = p.segments
instance IsString Path where
  fromString = path . cs


type Segment = Text


cleanSegment :: Segment -> Segment
cleanSegment = T.dropWhileEnd (== '/') . T.dropWhile (== '/')


path :: Text -> Path
path p =
  let segments = filter (not . T.null) $ T.splitOn "/" $ T.dropWhile (== '/') p
      isRoot = "/" `T.isPrefixOf` p
   in Path{isRoot, segments}


pathUri :: Path -> URI
pathUri p =
  URI
    { uriPath = cs $ pathToText p
    , uriScheme = mempty
    , uriAuthority = Nothing
    , uriQuery = mempty
    , uriFragment = mempty
    }


uriToText :: URI -> Text
uriToText u = cs $ uriToString id u ""


pathToText :: Path -> Text
pathToText p =
  pathPrefix p <> T.intercalate "/" (fmap cleanSegment p.segments)
 where
  pathPrefix :: Path -> Text
  pathPrefix p' =
    if p'.isRoot then "/" else ""
