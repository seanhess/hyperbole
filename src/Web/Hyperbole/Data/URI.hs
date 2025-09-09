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
  , renderQuery
  , Query
  , QueryItem
  , (./.)
  , (.?.)
  , cleanSegment
  , Endpoint (..)
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
import System.FilePath (normalise, (</>))


-- Constructors ------------------------------------------
-- see `uri` for static URIs

-- Operators -----------------------------------------------

-- maybe lets not care about leading slashes at all until rendering
(./.) :: URI -> Path -> URI
u ./. p =
  u{Network.uriPath = addLeadingSlash $ normalise (u.uriPath </> newPath)}
 where
  newPath = cs $ pathToText False p

  addLeadingSlash pth =
    case take 1 pth of
      "/" -> pth
      _ -> '/' : pth


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

newtype Path = Path {segments :: [Segment]}
  deriving (Show, Eq)
instance IsList Path where
  type Item Path = Segment
  fromList = Path . filter (not . T.null)
  toList p = p.segments
instance IsString Path where
  fromString = path . cs


type Segment = Text


cleanSegment :: Segment -> Segment
cleanSegment = T.dropWhileEnd (== '/') . T.dropWhile (== '/')


path :: Text -> Path
path p =
  fromList $ T.splitOn "/" $ T.dropWhile (== '/') p


pathUri :: Path -> URI
pathUri p =
  URI
    { uriPath = cs $ pathToText True p
    , uriScheme = mempty
    , uriAuthority = Nothing
    , uriQuery = mempty
    , uriFragment = mempty
    }


uriToText :: URI -> Text
uriToText u = cs $ uriToString id u ""


pathToText :: Bool -> Path -> Text
pathToText isRoot p =
  pathPrefix <> T.intercalate "/" (fmap cleanSegment p.segments)
 where
  pathPrefix :: Text
  pathPrefix =
    if isRoot then "/" else ""


-- | A URI with a phantom type to distinguish different endpoints
newtype Endpoint a = Endpoint {uri :: URI}
