{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Docs.Markdown
  ( markdocs
  , markdump
  , nodeToView
  , embedFile
  ) where

import CMark
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Example.Style qualified as Style
import Web.Atomic.CSS
import Web.Hyperbole.Data.URI
import Web.Hyperbole.View

markdocs :: ByteString -> View c ()
markdocs md = do
  nodeToView $ commonmarkToNode [] $ cs md

markdump :: ByteString -> View c ()
markdump md = do
  code $ cs $ show $ commonmarkToNode [] $ cs md

nodeToView :: Node -> View c ()
nodeToView (Node _mpos typ childs) = do
  let inner = mapM_ nodeToView childs
  case typ of
    PARAGRAPH -> el inner
    TEXT t -> text t
    CODE t -> do
      inlineCode t ~ color hackageSymbolColor
    HEADING _lvl ->
      el ~ bold $ inner
    LINK url _title ->
      case parseURIReference (cs url) of
        Nothing -> text $ "INVALID URI: " <> url
        Just u -> link u ~ Style.link @ att "target" "_blank" $ inner
    LIST (ListAttributes ORDERED_LIST _ _ _) ->
      tag "ol" ~ list Decimal . pad (L 32) $ inner
    LIST (ListAttributes BULLET_LIST _ _ _) ->
      tag "ul" ~ list Disc . pad (L 32) $ inner
    ITEM -> tag "li" inner
    _ -> inner
 where
  -- symbolColor = "#c2185b"
  hackageSymbolColor :: HexColor
  hackageSymbolColor = "#9e358f"

hackageDocsURI :: URI
hackageDocsURI = [uri|https://hackage-content.haskell.org/package/hyperbole-0.5.0/docs/Web-Hyperbole.html|]

inlineCode :: Text -> View c ()
inlineCode cd
  | cd `elem` typeKeywords = linkSymbolDocs cd typeFrag
  | cd `elem` valueKeywords = linkSymbolDocs cd valFrag
  | otherwise = tag' True "code" $ text cd
 where
  typeFrag t = "#t:" <> cs t
  valFrag v = "#v:" <> cs v

linkSymbolDocs :: Text -> (Text -> String) -> View c ()
linkSymbolDocs sym frag = do
  link (hackageDocsURI{uriFragment = frag sym}) @ att "target" "_blank" $ do
    tag' True "code" $ text sym

typeKeywords :: [Text]
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
  ]

valueKeywords :: [Text]
valueKeywords =
  [ "context"
  , "update"
  , "form"
  , "hyper"
  , "request"
  ]

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
