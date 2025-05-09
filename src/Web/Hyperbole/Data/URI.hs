module Web.Hyperbole.Data.URI
  ( URI (..)
  , Segment
  , Path (..)
  , path
  , uri
  , pathUri
  , uriToText
  , pathToText
  , cleanSegment
  , (./.)
  )
where

import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsList (..))
import Network.URI (URI (..), uriToString)
import Network.URI.Static (uri)
import System.FilePath ((</>))


data Path = Path
  { isRoot :: Bool
  , segments :: [Segment]
  }
  deriving (Show, Eq)
instance IsList Path where
  type Item Path = Segment
  fromList ss = Path True ss
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
    , uriScheme = ""
    , uriAuthority = Nothing
    , uriQuery = ""
    , uriFragment = ""
    }


uriToText :: URI -> Text
uriToText u = cs $ uriToString id u ""


pathPrefix :: Path -> Text
pathPrefix p =
  if p.isRoot then "/" else ""


pathToText :: Path -> Text
pathToText p =
  pathPrefix p <> T.intercalate "/" (fmap cleanSegment p.segments)


(./.) :: URI -> Path -> URI
u ./. p =
  let newPath = cs $ pathToText p
   in u{uriPath = if p.isRoot then newPath else u.uriPath </> newPath}
infixl 5 ./.

-- -- you can't create an isstring instance though....
-- -- hmmm....
-- -- I mean, they're doing that on purpose
--
-- relativeUri :: Text -> URI
-- relativeUri t =
--   s <- scheme
--   d <- domain s
--   ps <- paths
--   q <- query
--   pure $ Url{scheme = s, domain = d, path = ps, query = q}
--  where
--   parse :: (State Text :> es) => (Char -> Bool) -> Eff es Text
--   parse b = do
--     inp <- get
--     let match = T.takeWhile b inp
--         rest = T.dropWhile b inp
--     put rest
--     pure match
--
--   string :: (State Text :> es) => Text -> Eff es (Maybe Text)
--   string pre = do
--     inp <- get
--     case T.stripPrefix pre inp of
--       Nothing -> pure Nothing
--       Just rest -> do
--         put rest
--         pure (Just pre)
--
--   -- it's either scheme AND domain, or relative path
--   scheme = do
--     http <- string "http://"
--     https <- string "https://"
--     pure $ fromMaybe "" $ http <|> https
--
--   domain "" = pure ""
--   domain _ = parse (not . isDomainSep)
--
--   pathText :: (State Text :> es) => Eff es Text
--   pathText = parse (not . isQuerySep)
--
--   paths :: (State Text :> es) => Eff es [Segment]
--   paths = do
--     p <- pathText
--     pure $ pathSegments p
--
--   query :: (State Text :> es) => Eff es Query
--   query = do
--     q <- parse (/= '\n')
--     pure $ parseQuery $ encodeUtf8 q
--
--   isDomainSep '/' = True
--   isDomainSep _ = False
--
--   isQuerySep '?' = True
--   isQuerySep _ = False
--
--
-- renderUrl :: Url -> Text
-- renderUrl u = u.scheme <> u.domain <> renderPath u.path <> decodeUtf8 (renderQuery True u.query)
--
-- j
