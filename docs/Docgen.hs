module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.Char (isAlpha, isSpace)
import Data.List qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.State.Static.Local
import System.Directory
import System.Environment (getArgs)
import System.FilePath


-- Manually run this on the entire repository
-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [original, input, output] -> do
--         src <- readSource input
--         expanded <- expandFile src
--         let linePragma = T.pack $ "{-# LINE 1 \"" ++ original ++ "\" #-}"
--         let final = SourceCode [linePragma] <> expanded
--         T.writeFile output $ T.unlines final.lines
--     _ -> error "Usage (Hyperbole Internal Only): docgen src/MyModule.hs /tmp/input/MyModule.hs /build/output/Original.hs"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Docgen: all source"
      fs <- relativeSourceFiles "./src"
      mapM_ putStrLn fs
      pure ()
    files -> do
      putStrLn $ "Docgen: " <> show files
      forM_ files $ \file -> do
        src <- readSourceCode file
        src' <- expandFile src
        T.writeFile file src'.contents
        pure ()


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


srcDirectory :: FilePath
srcDirectory = "./src"


demoDirectory :: FilePath
demoDirectory = "./demo"


-- expandFile :: SourceCode -> IO SourceCode
-- expandFile (SourceCode src) =
--   case T.breakOn "{-$" src of
--     (end, "") -> pure $ SourceCode end
--     (start, rest) -> do
--       let (commentFragment, restInner) = T.breakOn "-}" rest
--       let next = T.drop 2 restInner -- drop the comment chars
--       let comment = commentFragment <> "-}"
--       pure SourceCode start <> next

expandFile :: SourceCode -> IO SourceCode
expandFile src = do
  parsed <- parseSource src

  putStrLn "\nPARSED"
  putStrLn "============"
  mapM_ print parsed

  expanded <- mconcat <$> mapM expandBlock parsed

  putStrLn "\nEXPANDED"
  putStrLn "============"
  mapM_ print expanded

  -- let final = dropOldHaddockBlocks expanded
  -- putStrLn "\nFINAL"
  -- putStrLn "============"
  -- mapM_ print final

  -- pure $ allToSourceCode $ dropOldHaddockBlocks final
  pure $ allToSourceCode expanded


-- dropOldHaddockBlocks :: [ParsedSource] -> [ParsedSource]
-- dropOldHaddockBlocks ps =
--   case ps of
--     [] -> []
--     (CommentTarget t : OtherCode newlines : HaddockBlock h1 : OtherCode _ : HaddockBlock _ : rest) ->
--       CommentTarget t : OtherCode newlines : HaddockBlock h1 : rest
--     (ps1 : rest) -> ps1 : dropOldHaddockBlocks rest

expandBlock :: ParsedSource -> IO [ParsedSource]
expandBlock p =
  case p of
    OtherCode t -> pure [OtherCode t]
    HaddockBlock t -> pure [HaddockBlock t]
    CommentTarget t -> do
      haddock <- haddockBlock t
      pure [CommentTarget t, OtherCode "\n\n\n", haddock]
 where
  haddockBlock :: Text -> IO ParsedSource
  haddockBlock t = do
    let lns = T.lines t
    out <- mconcat <$> mapM expandLine lns
    pure $ HaddockBlock $ "{- |" <> T.drop 4 (T.dropWhileEnd (== '\n') (T.unlines out)) <> "\n"


allToSourceCode :: [ParsedSource] -> SourceCode
allToSourceCode = mconcat . fmap toSourceCode


data ParsedSource
  = OtherCode Text
  | CommentTarget Text
  | HaddockBlock Text
  deriving (Show, Eq)


toSourceCode :: ParsedSource -> SourceCode
toSourceCode p =
  case p of
    OtherCode t -> SourceCode t
    CommentTarget t -> SourceCode t
    HaddockBlock t -> SourceCode t


parseSource :: SourceCode -> IO [ParsedSource]
parseSource sourceCode =
  runParser sourceCode parseFile
 where
  runParser :: SourceCode -> Eff [State SourceCode, IOE] [ParsedSource] -> IO [ParsedSource]
  runParser src = runEff . evalState src

  parseFile :: (State SourceCode :> es) => Eff es [ParsedSource]
  parseFile = do
    ocs <- parseOtherCode
    aps <- parseAllConcat $ do
      cmts <- parseCommentTarget
      next <- parseOtherCode
      pure $ cmts <> next
    pure $ ocs <> aps

  parseOtherCode :: (State SourceCode :> es) => Eff es [ParsedSource]
  parseOtherCode = do
    code <- takeUntil "{- $"
    case code of
      "" -> pure []
      _ -> pure [OtherCode code]

  parseAllConcat :: (State SourceCode :> es) => Eff es [a] -> Eff es [a]
  parseAllConcat p = do
    as <- p
    empty <- isEmpty
    next <-
      if empty
        then pure []
        else parseAllConcat p
    pure $ as <> next

  -- only when we know a comment target is there?
  parseCommentTarget :: (State SourceCode :> es) => Eff es [ParsedSource]
  parseCommentTarget = do
    comment <- takeIncluding "-}"
    skipWhitespace
    skipHaddock -- skip any haddock immediately following this
    skipWhitespace
    pure [CommentTarget comment]

  isEmpty :: (State SourceCode :> es) => Eff es Bool
  isEmpty = do
    SourceCode src <- get
    pure $ T.null src

  -- parseTarget :: (State SourceCode :> es) => Eff es ParsedSource
  -- parseTarget = _

  -- case breakOn commentTargetMarker src of
  --   (end, "") -> pure [OtherCode end]
  --   (start, rest) -> do
  --     -- rest includes {-$
  --     let (commentFragment, restInner) = T.breakOn "-}" rest
  --         next = T.drop 2 restInner -- drop "-}" from next part
  --         comment = commentFragment <> "-}" -- add to the comment block
  --     nextSources <- parseSource (SourceCode next)
  --
  --     pure $ OtherCode start : CommentTarget comment : nextSources

  -- -- parses until the end of a comment
  -- breakComment :: Text -> (Text, Text)
  -- breakComment = breakAfter "-}"

  -- parseTarget :: Text -> [ParsedSource]
  -- parseTarget start =
  --   case breakComment start of
  --     (end, "") -> [OtherCode end]
  --     (comment, rest) -> _
  -- now, we need to keep going!
  -- let next = T.stripStart rest
  -- pure ()
  -- case T.take 4 (T.stripStart rest) of
  --     "{- |" -> _
  --     code -> OtherCode rest
  --  in -- do we have whitespace?
  --     -- do we have code?
  --     -- do we have a haddock next?
  --     -- comment is complete
  --     -- let pcmt = case T.take 4 comment of
  --     --       "{- $" -> CommentTarget comment
  --     --       "{- |" -> HaddockBlock comment
  --     --       other -> OtherCode other
  --     [CommentTarget comment, OtherCode rest]

  skipUntil :: (State SourceCode :> es) => Text -> Eff es ()
  skipUntil match = do
    _ <- takeUntil match
    pure ()

  takeUntil :: (State SourceCode :> es) => Text -> Eff es Text
  takeUntil match = do
    SourceCode input <- get
    let (before, after) = T.breakOn match input
    put $ SourceCode after
    pure before

  takeIncluding :: (State SourceCode :> es) => Text -> Eff es Text
  takeIncluding match = do
    SourceCode input <- get
    let (before, after) = T.breakOn match input
    put $ SourceCode $ T.drop (T.length match) after
    -- only add it back to the before if we had a match
    case after of
      "" -> pure before
      _ -> pure $ before <> match

  skipWhitespace :: (State SourceCode :> es) => Eff es ()
  skipWhitespace = do
    modify (\(SourceCode t) -> SourceCode $ T.stripStart t)

  skipHaddock :: (State SourceCode :> es) => Eff es ()
  skipHaddock = do
    SourceCode src <- get
    when ("{- |" `T.isPrefixOf` src) $ do
      _ <- takeIncluding "-}"
      pure ()


commentTargetMarker :: Text
commentTargetMarker = "{- $"


haddockMarker :: Text
haddockMarker = "{- |"


readSource :: FilePath -> IO SourceCode
readSource pth = do
  inp <- T.readFile pth
  pure $ SourceCode inp


writeSource :: FilePath -> FilePath -> SourceCode -> IO ()
writeSource tmpDir relPath src = do
  let pth = tmpDir </> cleanRelativeDir relPath
  -- putStrLn $ "WRITE " <> pth <> " " <> show (length src.lines)
  createDirectoryIfMissing True $ takeDirectory pth
  T.writeFile pth src.contents
 where
  cleanRelativeDir =
    dropWhile (== '/') . dropWhile (== '.')


data Macro
  = Embed
  { moduleName :: ModuleName
  , definition :: TopLevelDefinition
  }
  deriving (Eq)
newtype SourceCode = SourceCode {contents :: Text}
  deriving newtype (Monoid, Semigroup)
instance Show Macro where
  -- show (Example p) = "Example " <> show p
  show (Embed mn def) = "Embed " <> show mn <> " " <> show def


newtype ModuleName = ModuleName Text
  deriving newtype (Eq, Show)


newtype TopLevelDefinition = TopLevelDefinition Text
  deriving newtype (Show, Eq)


-- > EMBED Example/Docs/BasicPage.hs page
expandLine :: Text -> IO [Text]
expandLine line = do
  case parseMacro line of
    Nothing -> do
      pure [line]
    Just (pre, Embed src def) -> do
      exists <- doesDirectoryExist demoDirectory
      if exists
        then expandEmbed src pre def
        else pure []
 where
  -- Just (pre, Example src) -> do
  --   expandExample src pre

  parseMacro :: Text -> Maybe (Text, Macro)
  parseMacro inp = do
    parseEmbed inp -- <|> parseExample inp

  -- parseExample l = do
  --   case T.splitOn "#EXAMPLE " l of
  --     [prefix, src] -> do
  --       pure (prefix, Example $ path src)
  --     _ -> Nothing

  parseEmbed l = do
    case T.splitOn "#EMBED " l of
      [prefix, info] -> do
        (mn, definition) <- splitSrcDef $ T.dropWhile (== ' ') info
        pure (prefix, Embed mn definition)
      _ -> Nothing

  splitSrcDef inp =
    let (mn, def) = T.breakOn " " inp
     in pure (ModuleName mn, TopLevelDefinition $ T.drop 1 def)


modulePath :: ModuleName -> FilePath
modulePath (ModuleName mn) = cs $ T.replace "." "/" mn <> ".hs"


readSourceCode :: FilePath -> IO SourceCode
readSourceCode p = do
  src <- T.readFile p
  pure $ SourceCode src


expandEmbed :: ModuleName -> Text -> TopLevelDefinition -> IO [Text]
expandEmbed mn pfx def = do
  let src = modulePath mn
  -- putStrLn $ "  embed: " <> src
  source <- readSourceCode $ demoDirectory </> src
  expanded <- requireTopLevel def source
  pure $ fmap markupLine expanded
 where
  requireTopLevel :: TopLevelDefinition -> SourceCode -> IO [Text]
  requireTopLevel tld sc =
    case findTopLevel tld sc of
      [] -> fail $ "Could not find: " <> show (Embed mn def) <> " " <> show def
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
  let rest = dropWhile (not . isTopLevel) $ T.lines source.contents
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
