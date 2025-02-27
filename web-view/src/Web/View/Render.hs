{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Web.View.Render where

import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack, toLower)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as LE
import HTMLEntities.Text qualified as HE
import Web.View.Types
import Web.View.View (View, ViewState (..), runView)


{- | Renders a 'View' as HTML with embedded CSS class definitions

>>> renderText $ el bold "Hello"
<style type='text/css'>.bold { font-weight:bold }</style>
<div class='bold'>Hello</div>
-}
renderText :: View () () -> Text
renderText = renderText' ()


renderLazyText :: View () () -> L.Text
renderLazyText = L.fromStrict . renderText


renderLazyByteString :: View () () -> BL.ByteString
renderLazyByteString = LE.encodeUtf8 . renderLazyText


data Line = Line {end :: LineEnd, indent :: Int, text :: Text}
  deriving (Show, Eq)


data LineEnd
  = Newline
  | Inline
  deriving (Eq, Show)


-- | Render lines to text
renderLines :: [Line] -> Text
renderLines = snd . foldl' nextLine (False, "")
 where
  nextLine :: (Bool, Text) -> Line -> (Bool, Text)
  nextLine (newline, t) l = (nextNewline l, t <> currentLine newline l)

  currentLine :: Bool -> Line -> Text
  currentLine newline l
    | newline = "\n" <> spaces l.indent <> l.text
    | otherwise = l.text

  nextNewline l = l.end == Newline

  spaces n = T.replicate n " "


{- | Render with the specified view context

> renderText' () $ el bold "Hello"
-}
renderText' :: c -> View c () -> Text
renderText' c vw =
  let vst = runView c vw
      css = renderCSS vst.css
   in renderLines $ addCss css $ mconcat $ fmap (renderContent 2) vst.contents
 where
  addCss :: [Line] -> [Line] -> [Line]
  addCss [] cnt = cnt
  addCss css cnt = do
    styleLines css <> (Line Newline 0 "" : cnt)

  styleLines :: [Line] -> [Line]
  styleLines css =
    [Line Newline 0 "<style type='text/css'>"]
      <> css
      <> [Line Newline 0 "</style>"]


renderContent :: Int -> Content -> [Line]
renderContent ind (Node t) = renderTag ind t
renderContent _ (Text t) = [Line Inline 0 $ HE.text t]
renderContent _ (Raw t) = [Line Newline 0 t]


renderTag :: Int -> Element -> [Line]
renderTag ind tag =
  case tag.children of
    [] ->
      -- auto closing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [line $ open <> htmlAtts (flatAttributes tag) <> ">" <> close]
    -- single text node
    [Text t] ->
      -- SINGLE text node, just display it indented
      [line $ open <> htmlAtts (flatAttributes tag) <> ">" <> HE.text t <> close]
    _ ->
      mconcat
        [ [line $ open <> htmlAtts (flatAttributes tag) <> ">"]
        , fmap (addIndent ind) $ htmlChildren tag.children
        , [line close]
        ]
 where
  open = "<" <> tag.name
  close = "</" <> tag.name <> ">"

  line t =
    if tag.inline
      then Line Inline 0 t
      else Line Newline 0 t

  htmlChildren :: [Content] -> [Line]
  htmlChildren cts =
    mconcat $
      fmap (renderContent ind) cts

  htmlAtts :: FlatAttributes -> Text
  htmlAtts (FlatAttributes []) = ""
  htmlAtts (FlatAttributes as) =
    " "
      <> T.unwords (map htmlAtt $ M.toList as)
   where
    htmlAtt (k, v) =
      k <> "=" <> "'" <> HE.text v <> "'"


addIndent :: Int -> Line -> Line
addIndent n (Line e ind t) = Line e (ind + n) t


renderCSS :: CSS -> [Line]
renderCSS = mapMaybe renderClass . M.elems
 where
  renderClass :: Class -> Maybe Line
  renderClass c | M.null c.properties = Nothing
  renderClass c =
    let sel = selectorText c.selector
        props = intercalate "; " (map renderProp $ M.toList c.properties)
     in Just $ Line Newline 0 $ [i|#{sel} { #{props} }|] & addMedia c.selector.media

  addMedia Nothing css = css
  addMedia (Just m) css =
    let mc = mediaCriteria m
     in [i|@media #{mc} { #{css} }|]

  mediaCriteria :: Media -> Text
  mediaCriteria (MinWidth n) = [i|(min-width: #{n}px)|]
  mediaCriteria (MaxWidth n) = [i|(max-width: #{n}px)|]

  renderProp :: (Text, StyleValue) -> Text
  renderProp (p, cv) = p <> ":" <> renderStyle cv

  renderStyle :: StyleValue -> Text
  renderStyle (StyleValue v) = pack v


indent :: Text -> Text
indent t = "  " <> t


-- | The css selector for this style
selectorText :: Selector -> Text
selectorText s =
  let classAttributeName = HE.text (attributeClassName s).text
   in ancestor s.ancestor <> "." <> addPseudo s.pseudo classAttributeName <> child s.child
 where
  ancestor Nothing = ""
  ancestor (Just p) = "." <> HE.text p <> " "

  -- ":" is treated as a pseudo selector. We want to use prefixed pseudos in the name as part of the name
  -- so we must escape the colon
  addPseudo Nothing c = c
  addPseudo (Just p) c =
    T.replace ":" "\\:" c <> ":" <> pseudoSuffix p

  child Nothing = ""
  child (Just (ChildWithName c)) =
    " > ." <> HE.text c
  child (Just AllChildren) =
    " > *"

  pseudoSuffix :: Pseudo -> Text
  pseudoSuffix Even = "nth-child(even)"
  pseudoSuffix Odd = "nth-child(odd)"
  pseudoSuffix p = pseudoText p


-- | Unique name for the class, as seen in the element's class attribute
attributeClassName :: Selector -> ClassName
attributeClassName sel =
  addMedia sel.media . addPseudo sel.pseudo . addAncestor sel.ancestor . addChild sel.child $ sel.className
 where
  addAncestor :: Maybe Ancestor -> ClassName -> ClassName
  addAncestor Nothing cn = cn
  addAncestor (Just a) cn = className a <> "-" <> cn

  addChild :: Maybe ChildCombinator -> ClassName -> ClassName
  addChild Nothing cn = cn
  addChild (Just (ChildWithName child)) cn = cn <> "-" <> className child
  addChild (Just AllChildren) cn = cn <> "-all"

  addPseudo :: Maybe Pseudo -> ClassName -> ClassName
  addPseudo Nothing cn = cn
  addPseudo (Just p) cn =
    className (pseudoText p) <> ":" <> cn

  addMedia :: Maybe Media -> ClassName -> ClassName
  addMedia Nothing cn = cn
  addMedia (Just (MinWidth n)) cn =
    "mmnw" <> fromString (show n) <> "-" <> cn
  addMedia (Just (MaxWidth n)) cn =
    "mmxw" <> fromString (show n) <> "-" <> cn


-- classNameAddAncestor :: Ancestor -> ClassName -> ClassName
-- classNameAddAncestor a cn =
--   ClassName a <> "-" <> cn
--
--
-- classNameAddChild :: ChildCombinator -> ClassName -> ClassName
-- classNameAddChild cc cn =
--   case cc of
--     ChildWithName child -> cn <> "-" <> ClassName child
--     AllChildren -> cn <> "-all"
--
-- classNameAddPseudo :: Pseudo -> ClassName -> ClassName
-- classNameAddPseudo p cn =
--     className (pseudoText p) <> ":" <> cn
--

pseudoText :: Pseudo -> Text
pseudoText p = toLower $ pack $ show p


-- | The 'Web.View.Types.Attributes' for an element, inclusive of class.
flatAttributes :: Element -> FlatAttributes
flatAttributes t =
  FlatAttributes $
    addClass t.attributes.classes t.attributes.other
 where
  addClass css atts
    | M.null css = atts
    | otherwise = M.insert "class" (classAttValue $ M.elems css) atts

  classAttValue :: [Class] -> Text
  classAttValue cx =
    T.unwords $ fmap ((.text) . attributeClassName . (.selector)) cx
