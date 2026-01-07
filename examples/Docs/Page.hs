{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Docs.Page
  ( PageAnchor (..)
  , sourceLink
  , example
  , example'
  , section
  , sectionA
  , section'
  , Cyber.embed
  , Cyber.quote
  ) where

import App.Route
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Docs.Examples
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Language.Haskell.TH
import Text.Casing (fromHumps, toWords)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

class PageAnchor n where
  pageAnchor :: n -> Text
  default pageAnchor :: n -> Text
  pageAnchor = T.toLower . T.replace " " "-" . sectionTitle

  sectionTitle :: n -> Text
  default sectionTitle :: (Show n) => n -> Text
  sectionTitle = cs . toWords . fromHumps . show

  navEntry :: n -> Text
  default navEntry :: n -> Text
  navEntry = sectionTitle

  subnav :: [n]
  default subnav :: (Enum n, Bounded n) => [n]
  subnav = [minBound .. maxBound]

instance PageAnchor () where
  subnav = []

-- Sections ----------------------------------------------------------------------

-- TODO: remove prefix if absolute path
sourceLink :: Path -> View c ()
sourceLink p =
  link sourceUrl ~ fontSize 14 @ att "target" "_blank" $ do
    text "</> Source"
 where
  sourceUrlBase = [uri|https://github.com/seanhess/hyperbole/blob/main/examples/|]
  sourceUrl = sourceUrlBase ./. p

example :: ModuleSource -> View c () -> View c ()
example (ModuleSource e) = example' (path $ cs e)

example' :: Path -> View c () -> View c ()
example' p cnt = do
  el ~ stack . Cyber.font $ do
    col ~ Cyber.embed $ cnt
    sourceLink p ~ popup (TR (-10) 0) . pad (XY 8 2) . bg PrimaryLight . color White . hover (bg Primary) -- . pad (TRBL 0 20 0 10) . border (L 3) . borderColor PrimaryLight . Cyber.clip 10

section :: AppRoute -> View c () -> View c ()
section r = section' (routeTitle r)

section' :: Text -> View c () -> View c ()
section' t cnt = do
  tag "section" ~ gap 10 . flexCol $ do
    row $ do
      el ~ bold . fontSize 28 . Cyber.font . Style.uppercase $ text t
    cnt

sectionA :: (PageAnchor n) => n -> View c () -> View c ()
sectionA n =
  section' (sectionTitle n)
    @ att "id" (pageAnchor n)

-- type Fragment = String
--
-- hackage :: Fragment -> Text -> View c ()
-- hackage uriFragment txt = do
--   let docs = [uri|https://hackage-content.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|]
--   link docs{uriFragment} @ att "target" "_blank" ~ Style.link $ do
--     el ~ iconInline $ do
--       Icon.bookOpen
--       text txt
