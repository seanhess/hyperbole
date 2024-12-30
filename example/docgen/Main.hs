module Main where

import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isSpace)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath

main :: IO ()
main = do
  let tmpDir = "/tmp/docs"
  copyExtraFilesTo tmpDir
  expandSourcesTo tmpDir

test :: IO ()
test = do
  putStrLn "TEST"
  src <- readSource "../src/Web/Hyperbole.hs"
  SourceCode lns <- expandFile src
  mapM_ print lns

expandSourcesTo :: FilePath -> IO ()
expandSourcesTo tmpDir = do
  allFiles <- relativeSourceFiles "../src"
  print allFiles
  mapM_ (expandAndCopyFileTo tmpDir) allFiles

copyExtraFilesTo :: FilePath -> IO ()
copyExtraFilesTo tmpDir = do
  createDirectoryIfMissing True tmpDir
  copyFile "../hyperbole.cabal" (tmpDir </> "hyperbole.cabal")
  copyFile "../README.md" (tmpDir </> "README.md")
  copyFile "../CHANGELOG.md" (tmpDir </> "CHANGELOG.md")
  copyFile "../LICENSE" (tmpDir </> "LICENSE")
  createDirectoryIfMissing True (tmpDir </> "client/dist")
  copyFile "../client/dist/hyperbole.js" (tmpDir </> "client/dist/hyperbole.js")

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
  putStrLn dir
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

data Embed = Embed
  { definition :: TopLevelDefinition
  , prefix :: Text
  , sourceFile :: FilePath
  }
  deriving (Eq)
newtype SourceCode = SourceCode {lines :: [Text]}
instance Show Embed where
  show e = "Embed " <> e.sourceFile <> " " <> show e.prefix <> " " <> show e.definition

newtype TopLevelDefinition = TopLevelDefinition Text
  deriving newtype (Show, Eq)

expandFile :: SourceCode -> IO SourceCode
expandFile (SourceCode lns) =
  SourceCode . mconcat <$> mapM expandLine lns

expandLine :: Text -> IO [Text]
expandLine line = do
  case parseEmbed line of
    Nothing -> do
      pure [line]
    Just emb -> expandEmbed emb
 where
  parseEmbed inp = do
    let (prefix, rest) = T.break (== '#') inp
    info <- splitMacro rest
    (sourceFile, definition) <- splitSrcDef info
    pure $ Embed{sourceFile, definition, prefix}

  splitMacro inp =
    case T.splitAt 7 inp of
      ("#EMBED ", info) -> pure info
      _ -> Nothing

  splitSrcDef inp =
    let (src, def) = T.breakOn " " inp
     in pure (cs src, TopLevelDefinition $ T.drop 1 def)

expandEmbed :: Embed -> IO [Text]
expandEmbed embed = do
  print embed
  source <- T.readFile embed.sourceFile
  expanded <- requireTopLevel embed.definition (SourceCode $ T.lines source)
  pure $ fmap markupLine expanded
 where
  requireTopLevel :: TopLevelDefinition -> SourceCode -> IO [Text]
  requireTopLevel tld src =
    case findTopLevel tld src of
      [] -> fail $ "Could not find " <> show embed
      lns -> pure lns

  -- addPrefix line = embed.prefix <> line
  markupLine :: Text -> Text
  markupLine line =
    case embed.prefix of
      "" -> markupLineAt line
      _ -> markupLinePrefix line
  markupLineAt =
    T.replace "\"" "\\\"" . highlightTermsLine
  markupLinePrefix line =
    embed.prefix <> line

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
    , "basicDocument"
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
