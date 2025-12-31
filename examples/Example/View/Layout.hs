{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import Data.Text (Text)
import Example.AppRoute
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Icon as Icon (bookOpen, iconInline)
import Example.View.Navigation
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

exampleLayout :: AppRoute -> View c () -> View c ()
exampleLayout rt contents =
  el ~ grow $ do
    navigation rt ~ position Fixed . zIndex 1 . onDesktop leftMenu . onMobile topMenu
    col ~ pad 25 . gap 30 . onDesktop horizontal . onMobile vertical $ do
      contents
 where
  leftMenu = width menuWidth . left 0 . top 0 . bottom 0
  horizontal = margin (L menuWidth)
  vertical = margin (T menuHeight)

  topMenu = top 0 . right 0 . left 0

  menuWidth = 230
  menuHeight = 70

sourceLink :: Path -> View c ()
sourceLink p =
  link sourceUrl ~ Style.link . Cyber.font . fontSize 14 @ att "target" "_blank" $ do
    text "View Source"
 where
  sourceUrlBase = [uri|https://github.com/seanhess/hyperbole/blob/main/examples/|]
  sourceUrl = sourceUrlBase ./. p

embed :: (Styleable h) => CSS h -> CSS h
embed =
  pad 15 . gap 10 . bg White . flexCol . Cyber.clip 10

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

type Fragment = String

hackage :: Fragment -> Text -> View c ()
hackage uriFragment txt = do
  let docs = [uri|https://hackage-content.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|]
  link docs{uriFragment} @ att "target" "_blank" ~ Style.link $ do
    el ~ iconInline $ do
      Icon.bookOpen
      text txt
