{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Docs.Markdown
  ( markdocs
  , markdump
  , nodeToView
  , embedFile
  ) where

import App.Docs.Snippet
import App.Route
import CMark
import Data.Char (isSpace)
import Data.Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Example.Colors (magenta)
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Web.Atomic.CSS
import Web.Hyperbole.Data.URI
import Web.Hyperbole.HyperView.Input (route)
import Web.Hyperbole.Route
import Web.Hyperbole.View

markdocs :: Text -> View c ()
markdocs md = do
  nodeToView $ commonmarkToNode [] $ cs md

markdump :: Text -> View c ()
markdump md = do
  code $ cs $ show $ commonmarkToNode [] $ cs md

nodeToView :: Node -> View c ()
nodeToView (Node _mpos typ childs) = do
  let inner = mapM_ nodeToView childs
  case typ of
    -- DOCUMENT -> mapM nodeToView childs
    -- THEMATIC_BREAK -> _
    -- PARAGRAPH -> _
    -- BLOCK_QUOTE -> _
    -- HTML_BLOCK Text -> _
    -- CUSTOM_BLOCK OnEnter OnExit -> _
    -- CODE_BLOCK Info Text -> _
    -- HEADING Level -> _
    -- LIST ListAttributes -> _
    -- ITEM -> _
    -- TEXT Text -> _
    -- SOFTBREAK -> _
    -- LINEBREAK -> _
    -- HTML_INLINE Text -> _
    -- CUSTOM_INLINE OnEnter OnExit -> _
    -- CODE Text -> _
    -- EMPH -> _
    -- STRONG -> _
    -- LINK url title -> _
    -- IMAGE url title -> _
    PARAGRAPH -> el inner
    TEXT t -> text t
    CODE t -> do
      inlineCode t
    HEADING lvl ->
      el ~ bold . headerLevel lvl $ inner
    LINK url _title ->
      case matchRoute @AppRoute (path url) of
        Nothing -> do
          case parseURIReference (cs url) of
            Nothing -> text $ "INVALID URI: " <> url
            Just u ->
              link u ~ Style.link @ att "target" "_blank" $ inner
        Just r ->
          route r ~ Style.link $ inner
    LIST (ListAttributes ORDERED_LIST _ _ _) ->
      tag "ol" ~ list Decimal . pad (L 32) $ inner
    LIST (ListAttributes BULLET_LIST _ _ _) ->
      tag "ul" ~ list Disc . pad (L 32) $ inner
    ITEM -> tag "li" inner
    DOCUMENT -> inner
    CODE_BLOCK _info t ->
      snippet $ raw t
    BLOCK_QUOTE -> el ~ Cyber.quote $ inner
    HTML_BLOCK t -> raw t
    SOFTBREAK -> inner
    EMPH -> tag' True "span" ~ italic $ inner
    x ->
      -- inner
      raw $ cs $ show x
 where
  headerLevel lvl =
    case lvl of
      1 -> fontSize 24
      2 -> fontSize 20
      _ -> fontSize 16

hackageDocsURI :: URI
hackageDocsURI = [uri|https://hackage-content.haskell.org/package/hyperbole-0.5.0/docs/Web-Hyperbole.html|]

inlineCode :: Text -> View c ()
inlineCode cd
  | cd `elem` typeKeywords = linkSymbolDocs cd typeFrag ~ color hackageSymbolColor
  | cd `elem` valueKeywords = linkSymbolDocs cd valFrag ~ color hackageSymbolColor
  | otherwise = tag' True "code" ~ color magenta $ text cd
 where
  typeFrag t = "#t:" <> cs t
  valFrag v = "#v:" <> cs v

  hackageSymbolColor :: HexColor
  hackageSymbolColor = "#9e358f"

linkSymbolDocs :: Text -> (Text -> String) -> View c ()
linkSymbolDocs sym frag = do
  link (hackageDocsURI{uriFragment = frag sym}) @ att "target" "_blank" $ do
    tag' True "code" $ text sym

typeKeywords :: Set Text
typeKeywords =
  [ "Page"
  , "View"
  , "HyperView"
  , "ViewId"
  , "ViewAction"
  , "ViewState"
  , "Action"
  , "Hyperbole"
  , "Effect"
  , "Query"
  , "Session"
  , "Require"
  , "Client"
  , "Request"
  , "Document"
  , "Path"
  , "Route"
  , "Eff"
  , "Page"
  , "Response"
  , "FromForm"
  , "Validated"
  ]

valueKeywords :: Set Text
valueKeywords =
  [ "context"
  , "update"
  , "form"
  , "validate"
  , "hyper"
  , "request"
  , "viewId"
  , "viewState"
  , "trigger"
  , "target"
  , "hyperState"
  , "runPage"
  , "document"
  , "routeRequest"
  , "matchRoute"
  , "liveApp"
  , "pushUpdate"
  , "onLoad"
  , "session"
  , "query"
  , "setQuery"
  , "setParam"
  , "param"
  , "modifyQuery"
  , "saveSession"
  , "deleteSession"
  , "quickStartDocument"
  , "search"
  , "dropdown"
  , "option"
  , "button"
  , "onClick"
  , "onKeyDown"
  , "onKeyUp"
  , "onMouseEnter"
  , "onMouseLeave"
  , "onInput"
  ]

embedFile :: FilePath -> Q Exp
embedFile p = do
  addDependentFile p
  lns :: [Text] <- runIO $ T.lines <$> T.readFile p
  exps :: [Exp] <- traverse expandLine lns
  e :: Exp <- listE (fmap pure exps)
  [|T.unlines $(pure e)|]

expandLine :: Text -> Q Exp
expandLine l = do
  let whitespace = T.takeWhile isSpace l
  case parseLineEmbed l of
    Just (mn, tld) -> do
      e <- embedSource' mn (isTopLevel tld) (isCurrentDefinition tld)
      [|T.stripEnd $ T.unlines $ fmap (whitespace <>) $(pure e)|]
    Nothing -> do
      t <- expandText l
      lift t

expandText :: (MonadFail m) => Text -> m Text
expandText t = do
  let segs = T.splitOn "[[" t
  es :: [Text] <- mapM checkLink segs
  pure $ mconcat es
 where
  checkLink :: (MonadFail m) => Text -> m Text
  checkLink l = do
    case T.breakOn "]]" l of
      (txt, "") -> pure txt
      (lnk, rest) -> do
        mdlnk <- routeLink lnk
        pure $ mdlnk <> T.dropWhile (== ']') rest

  routeLink :: (MonadFail m) => Text -> m Text
  routeLink l =
    case matchRoute @AppRoute (path l) of
      Nothing -> error $ "Could not find page link: " <> cs l <> " " <> show (path l)
      Just r -> pure $ "[" <> routeTitle r <> "](" <> uriToText (routeUri r) <> ")"
