{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isSpace)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (verbose)
import App.Route as Example
import System.Directory
import System.FilePath
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Route


main :: IO ()
main = do
  let tmpDir = "/tmp/hyperbole"
  copyExtraFilesTo tmpDir
  expandSourcesTo tmpDir
  putStrLn $ "COPY RECURSIVE: " <> (tmpDir <> "docs")
  copyDirectoryRecursive verbose "./docs" (tmpDir </> "docs")
  copyDirectoryRecursive verbose "./examples" (tmpDir </> "examples")


test :: IO ()
test = do
  src <- readSource "./src/Web/Hyperbole.hs"
  SourceCode lns <- expandFile src
  mapM_ print lns


expandSourcesTo :: FilePath -> IO ()
expandSourcesTo tmpDir = do
  allFiles <- relativeSourceFiles "./src"
  -- mapM_ (putStrLn . ("SOURCE " <>)) allFiles
  mapM_ (expandAndCopyFileTo tmpDir) allFiles


copyExtraFilesTo :: FilePath -> IO ()
copyExtraFilesTo tmpDir = do
  createDirectoryIfMissing True tmpDir
  copyFile "./cabal.project" (tmpDir </> "cabal.project")
  copyFile "./hyperbole.cabal" (tmpDir </> "hyperbole.cabal")
  copyFile "./README.md" (tmpDir </> "README.md")
  copyFile "./CHANGELOG.md" (tmpDir </> "CHANGELOG.md")
  copyFile "./LICENSE" (tmpDir </> "LICENSE")
  createDirectoryIfMissing True (tmpDir </> "client/dist")
  copyFile "./client/dist/hyperbole.js" (tmpDir </> "client/dist/hyperbole.js")
  copyFile "./client/dist/hyperbole.js.map" (tmpDir </> "client/dist/hyperbole.js.map")
  createDirectoryIfMissing True (tmpDir </> "client/util")
  copyFile "./client/util/live-reload.js" (tmpDir </> "client/util/live-reload.js")


expandAndCopyFileTo :: FilePath -> FilePath -> IO ()
expandAndCopyFileTo tmpDir pth = do
  src <- readSource pth
  expanded <- expandFile src
  writeSource tmpDir pth expanded


readSource :: FilePath -> IO SourceCode
readSource pth = do
  inp <- T.readFile pth
  pure $ SourceCode $ T.lines inp


writeSource :: FilePath -> FilePath -> SourceCode -> IO ()
writeSource tmpDir relPath src = do
  let pth = tmpDir </> cleanRelativeDir relPath
  createDirectoryIfMissing True $ takeDirectory pth
  T.writeFile pth $ T.unlines src.lines
  putStrLn $ "WROTE to " <> pth <> " " <> show (length src.lines)
 where
  cleanRelativeDir =
    dropWhile (== '/') . dropWhile (== '.')


relativeSourceFiles :: FilePath -> IO [FilePath]
relativeSourceFiles dir = do
  contents <- tryDirectory dir
  let folders = filter isFolder contents
  let files = filter isSourceFile contents

  files' <- mapM (relativeSourceFiles . addDir) folders

  pure $ fmap addDir files <> mconcat files'
 where
  isSourceFile pth = takeExtension pth == ".hs"
  isFolder pth = takeExtension pth == ""
  addDir = (dir </>)
  tryDirectory pth = do
    res <- try $ listDirectory pth
    case res of
      Left (_ :: SomeException) -> do
        putStrLn $ "SKIPPED" <> pth
        pure []
      Right files -> pure files


data Macro
  = Embed
      { sourceFile :: FilePath
      , definition :: TopLevelDefinition
      }
  | Example Path
  deriving (Eq)
newtype SourceCode = SourceCode {lines :: [Text]}
instance Show Macro where
  show (Embed src def) = "Embed " <> src <> " " <> show def
  show (Example src) = "Example " <> show src


newtype TopLevelDefinition = TopLevelDefinition Text
  deriving newtype (Show, Eq)


expandFile :: SourceCode -> IO SourceCode
expandFile (SourceCode lns) =
  SourceCode . mconcat <$> mapM expandLine lns


-- * #EXAMPLE /simple


-- > EMBED Example/Docs/BasicPage.hs page
expandLine :: Text -> IO [Text]
expandLine line = do
  case parseMacro line of
    Nothing -> do
      pure [line]
    Just (pre, Embed src def) -> do
      expandEmbed src pre def
    Just (pre, Example src) -> do
      expandExample src pre
 where
  parseMacro :: Text -> Maybe (Text, Macro)
  parseMacro inp = do
    parseEmbed inp <|> parseExample inp

  parseExample l = do
    case T.splitOn "#EXAMPLE " l of
      [prefix, src] -> do
        pure (prefix, Example $ path src)
      _ -> Nothing

  parseEmbed l = do
    case T.splitOn "#EMBED " l of
      [prefix, info] -> do
        (sourceFile, definition) <- splitSrcDef $ T.dropWhile (== ' ') info
        pure (prefix, Embed sourceFile definition)
      _ -> Nothing

  splitSrcDef inp =
    let (src, def) = T.breakOn " " inp
     in pure (cs src, TopLevelDefinition $ T.drop 1 def)


-- look it up as a URI...
expandExample :: Path -> Text -> IO [Text]
expandExample p prefix = do
  let pre = if T.null prefix then "▶️ " else prefix
  r <- appRoute
  pure [pre <> "[" <> Example.routeTitle r <> "](" <> uriToText (exampleBaseURI ./. p) <> ")"]
 where
  appRoute :: IO AppRoute
  appRoute = do
    case matchRoute @AppRoute p of
      Nothing -> fail $ "Could not find example: " <> cs (pathToText False p)
      Just r -> pure r


exampleBaseURI :: URI
exampleBaseURI = [uri|https://hyperbole.live|]


expandEmbed :: FilePath -> Text -> TopLevelDefinition -> IO [Text]
expandEmbed src pfx def = do
  source <- T.readFile $ "./examples/" <> src
  expanded <- requireTopLevel def (SourceCode $ T.lines source)
  pure $ fmap markupLine expanded
 where
  requireTopLevel :: TopLevelDefinition -> SourceCode -> IO [Text]
  requireTopLevel tld sc =
    case findTopLevel tld sc of
      [] -> fail $ "Could not find: " <> show (Embed src def)
      lns -> pure lns

  -- addPrefix line = embed.prefix <> line
  markupLine :: Text -> Text
  markupLine line =
    case pfx of
      "" -> markupLineAt line
      _ -> markupLinePrefix line
  markupLineAt =
    T.replace "\"" "\\\"" . highlightTermsLine
  markupLinePrefix line =
    pfx <> line

highlightTermsLine :: Text -> Text
highlightTermsLine ln = mconcat $ fmap highlightWord $ T.groupBy isSameTerm ln
 where
  isSameTerm :: Char -> Char -> Bool
  isSameTerm c1 c2 =
    (isAlpha c1 && isAlpha c2)
      || (isSpace c1 && isSpace c2)

  highlightWord :: Text -> Text
  highlightWord w =
    if w `elem` terms
      then "'" <> w <> "'"
      else w

  terms :: [Text]
  terms =
    [ "HyperView"
    , "View"
    , "Action"
    , "update"
    , "hyper"
    , "Page"
    , "liveApp"
    , "quickStartDocument"
    , "runPage"
    , "run"
    , "ViewId"
    , "viewId"
    , "ViewAction"
    , "Eff"
    , "button"
    , "el"
    , "el_"
    , "Hyperbole"
    , "Route"
    , "routeRequest"
    , "route"
    , "layout"
    , "Response"
    , "ToParam"
    , "FromParam"
    , "Session"
    , "FromQuery"
    , "ToQuery"
    , "lookupParam"
    , "setParam"
    , "DefaultParam"
    , "Client"
    ]


-- returns lines of a top-level definition
findTopLevel :: TopLevelDefinition -> SourceCode -> [Text]
findTopLevel (TopLevelDefinition definition) source =
  let rest = dropWhile (not . isTopLevel) source.lines
   in dropWhileEnd isEmpty $ takeWhile isCurrentDefinition rest
 where
  isTopLevel = T.isPrefixOf definition
  isEmpty = T.null
  -- isBlankLine line = T.null $ T.strip line
  isCurrentDefinition line =
    isTopLevel line || not (isFullyOutdented line)
  dropWhileEnd p as =
    reverse $ dropWhile p $ reverse as


isFullyOutdented :: Text -> Bool
isFullyOutdented line =
  case cs (T.take 1 line) of
    "" -> False
    [c] -> not $ isSpace c
    _ -> False
