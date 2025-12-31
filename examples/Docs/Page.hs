{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Docs.Page where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Example.AppRoute
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Text.Casing (fromHumps, toWords)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

class ExampleSource a where
  exampleSource :: a -> Path

instance ExampleSource AppRoute where
  -- exampleSource (State s) = ["Example", "Page", "State", cs (show s) <> ".hs"]
  exampleSource (Contacts (Contact _)) = "Example/Page/Contact.hs"
  exampleSource (Contacts ContactsAll) = "Example/Page/Contacts.hs"
  -- routeSource (Intro (CSS CSSAll)) = ["Example", "Page", "Intro", "CSS.hs"]
  -- routeSource (Intro (CSS c)) = ["Example", "Page", "Intro", "CSS", cs (show c) <> ".hs"]
  exampleSource (Data SortableTable) = "Example/Page/DataLists/DataTable.hs"
  exampleSource (Data d) = ["Example", "Page", "DataLists", cs (show d) <> ".hs"]
  exampleSource (Forms f) = ["Example", "Page", "Forms", cs (show f) <> ".hs"]
  exampleSource r = ["Example", "Page", cs (show r) <> ".hs"]

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

sourceLink :: Path -> View c ()
sourceLink p =
  link sourceUrl ~ Style.link . fontSize 14 @ att "target" "_blank" $ do
    text "View Source"
 where
  sourceUrlBase = [uri|https://github.com/seanhess/hyperbole/blob/main/examples/|]
  sourceUrl = sourceUrlBase ./. p

embed :: (Styleable h) => CSS h -> CSS h
embed =
  pad 15 . gap 10 . bg White . flexCol . Cyber.clip 10 . Cyber.font

example :: (ExampleSource e) => e -> View c () -> View c ()
example e = example' (exampleSource e)

example' :: Path -> View c () -> View c ()
example' p cnt = do
  col ~ Cyber.font $ do
    col ~ embed $ cnt
    sourceLink p

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
