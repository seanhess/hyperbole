module Data.Snippet where

import Data.Char (isAlpha, isSpace)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedLines :: FilePath -> Int -> Int -> Q Exp
embedLines path start end = do
  addDependentFile path
  contents <- runIO (T.readFile path)
  let selected =
        T.unlines
          . take (end - start + 1)
          . drop (start - 1)
          . T.lines
          $ contents
  lift (T.unpack selected)

newtype TopLevelDefinition = TopLevelDefinition Text
  deriving newtype (Show, Eq, IsString)

newtype SourceCode = SourceCode {lines :: [Text]}

{- | A top-level definition as text

> snippet $(topLevel "examples/Example/Page/Concurrency.hs" "instance (Debug :> es) => HyperView Polling")
-}
embedTopLevel :: FilePath -> TopLevelDefinition -> Q Exp
embedTopLevel path tld = do
  embedSource path (isTopLevel tld) (isCurrentDefinition tld)

embedSource :: FilePath -> (Text -> Bool) -> (Text -> Bool) -> Q Exp
embedSource path isStart isCurrent = do
  src <- runIO $ readSourceCode path
  let lns = selectLines isStart isCurrent src
  case lns of
    [] -> fail "Missing embed"
    _ -> lift (T.unlines lns)

readSnippet :: FilePath -> TopLevelDefinition -> IO [Text]
readSnippet path tld = do
  src <- readSourceCode path
  pure $ findTopLevel tld src

readSourceCode :: FilePath -> IO SourceCode
readSourceCode path = SourceCode . T.lines <$> T.readFile path

-- returns lines of a top-level definition
findTopLevel :: TopLevelDefinition -> SourceCode -> [Text]
findTopLevel tld =
  selectLines (isTopLevel tld) (isCurrentDefinition tld)

-- isBlankLine line = T.null $ T.strip line

isCurrentDefinition :: TopLevelDefinition -> Text -> Bool
isCurrentDefinition tld line =
  isTopLevel tld line || not (isFullyOutdented line)

isTopLevel :: TopLevelDefinition -> Text -> Bool
isTopLevel (TopLevelDefinition def) line = T.isPrefixOf def $ T.dropWhile (== ' ') line

selectLines :: (Text -> Bool) -> (Text -> Bool) -> SourceCode -> [Text]
selectLines isStart isCurrent src =
  let rest = dropWhile (not . isStart) src.lines
   in dropWhileEnd isEmpty $ takeWhile isCurrent rest
 where
  isEmpty = T.null

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p as =
  reverse $ dropWhile p $ reverse as

isFullyOutdented :: Text -> Bool
isFullyOutdented line =
  case cs (T.take 1 line) of
    "" -> False
    [c] -> not $ isSpace c
    _ -> False
