module Main where

import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isSpace)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (verbose)
import Example.Route.AppRoute
import System.Directory
import System.FilePath


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
  createDirectoryIfMissing True (tmpDir </> "client/util")
  copyFile "./client/util/live-reload.js" (tmpDir </> "client/util/live-reload.js")


expandAndCopyFileTo :: FilePath -> FilePath -> IO ()
expandAndCopyFileTo tmpDir path = do
  src <- readSource path
  expanded <- expandFile src
  writeSource tmpDir path expanded


readSource :: FilePath -> IO SourceCode
readSource path = do
  inp <- T.readFile path
  pure $ SourceCode $ T.lines inp


writeSource :: FilePath -> FilePath -> SourceCode -> IO ()
writeSource tmpDir relPath src = do
  let path = tmpDir </> cleanRelativeDir relPath
  createDirectoryIfMissing True $ takeDirectory path
  T.writeFile path $ T.unlines src.lines
  putStrLn $ "WROTE to " <> path <> " " <> show (length src.lines)
 where
  cleanRelativeDir =
    dropWhile (== '/') . dropWhile (== '.')


relativeSourceFiles :: FilePath -> IO [FilePath]
relativeSourceFiles dir = do
  contents <- tryDirectory dir
  -- putStrLn dir
  let folders = filter isFolder contents
  let files = filter isSourceFile contents

  files' <- mapM (relativeSourceFiles . addDir) folders

  pure $ fmap addDir files <> mconcat files'
 where
  isSourceFile path = takeExtension path == ".hs"
  isFolder path = takeExtension path == ""
  addDir = (dir </>)
  tryDirectory path = do
    res <- try $ listDirectory path
    case res of
      Left (_ :: SomeException) -> do
        putStrLn $ "SKIPPED" <> path
        pure []
      Right files -> pure files


data Macro
  = Embed
      { sourceFile :: FilePath
      , prefix :: Text
      , definition :: TopLevelDefinition
      }
  | Example FilePath
  deriving (Eq)
newtype SourceCode = SourceCode {lines :: [Text]}
instance Show Macro where
  show (Embed src pfx def) = "Embed " <> src <> " " <> cs pfx <> " " <> show def
  show (Example src) = "Example " <> src


newtype TopLevelDefinition = TopLevelDefinition Text
  deriving newtype (Show, Eq)


expandFile :: SourceCode -> IO SourceCode
expandFile (SourceCode lns) =
  SourceCode . mconcat <$> mapM expandLine lns


expandLine :: Text -> IO [Text]
expandLine line = do
  case parseMacro line of
    Nothing -> do
      pure [line]
    Just (Embed src pfx def) -> do
      print (line, src, pfx, def)
      expandEmbed src pfx def
    Just (Example src) ->
      expandExample src
 where
  parseMacro inp = do
    let (prefix, rest) = T.break (== '#') inp
    splitMacro prefix rest

  splitMacro prefix inp = do
    case T.breakOn " " inp of
      ("#EMBED", info) -> do
        traceM (show (prefix, inp, info))
        (sourceFile, definition) <- splitSrcDef $ T.dropWhile (== ' ') info
        pure $ Embed sourceFile prefix definition
      ("#EXAMPLE", info) -> pure $ Example (cs $ T.dropWhile (== ' ') info)
      _ -> Nothing

  splitSrcDef inp =
    let (src, def) = T.breakOn " " inp
     in pure (cs src, TopLevelDefinition $ T.drop 1 def)


-- look it up as a URI...
expandExample :: FilePath -> IO [Text]
expandExample src = do
  putStrLn "CHECKING EXAMPLE"
  pure ["EXAMPLE GOES HERE"]


expandEmbed :: FilePath -> Text -> TopLevelDefinition -> IO [Text]
expandEmbed src pfx def = do
  source <- T.readFile $ "./examples/" <> src
  expanded <- requireTopLevel def (SourceCode $ T.lines source)
  pure $ fmap markupLine expanded
 where
  requireTopLevel :: TopLevelDefinition -> SourceCode -> IO [Text]
  requireTopLevel tld sc =
    case findTopLevel tld sc of
      [] -> fail $ "Could not find " <> show (Embed src pfx def)
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
