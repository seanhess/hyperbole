{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Version (showVersion)
import Example.AppRoute
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Icon as Icon (bookOpen, hamburger, iconInline)
import Paths_examples (version)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI
import Web.Hyperbole.View.Types (tag')

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
  link sourceUrl ~ Style.link . fontSize 14 @ att "target" "_blank" $ do
    text "View Source"
 where
  sourceUrlBase = [uri|https://github.com/seanhess/hyperbole/blob/main/examples/|]
  sourceUrl = sourceUrlBase ./. p

embed :: (Styleable h) => CSS h -> CSS h
embed =
  pad 15 . gap 10 . bg White . flexCol . Cyber.clip 10

example :: AppRoute -> View c () -> View c ()
example r = example' (routeSource r)

example' :: Path -> View c () -> View c ()
example' p cnt = do
  col $ do
    col ~ embed $ cnt
    sourceLink p

snippet :: View c () -> View c ()
snippet cnt = do
  tag' True "pre" ~ bg Light $ do
    tag' True "code" @ class_ "language-haskell" $ do
      cnt

section :: AppRoute -> View c () -> View c ()
section r = section' (routeTitle r)

section' :: Text -> View c () -> View c ()
section' t cnt =
  col ~ gap 10 $ do
    row $ do
      el ~ bold . fontSize 28 . Cyber.font . Style.uppercase $ text t
    cnt

exampleMenu :: AppRoute -> View c ()
exampleMenu current = do
  exampleLink Intro
  exampleLink (CSS CSSAll)
  exampleLink Concurrency
  exampleLink (State StateRoot)
  case current of
    State _ -> do
      exampleLink (State Actions) ~ sub
      exampleLink (State Effects) ~ sub
      exampleLink (State Query) ~ sub
      exampleLink (State Sessions) ~ sub
    _ -> none
  exampleLink Requests
  exampleLink (Data DataLists)
  case current of
    Data _ -> do
      exampleLink (Data SortableTable) ~ sub
      exampleLink (Data Autocomplete) ~ sub
      exampleLink (Data Filter) ~ sub
      exampleLink (Data LoadMore) ~ sub
    _ -> none
  exampleLink (Forms FormSimple)
  exampleLink Interactivity
  exampleLink Errors
  exampleLink OAuth2
  exampleLink Javascript
  exampleLink Advanced
  exampleLink (Examples BigExamples)
  case current of
    Examples _ ->
      completeExamples
    (Contacts _) ->
      completeExamples
    _ -> none
 where
  completeExamples = do
    exampleLink (Examples Todos) ~ sub
    exampleLink (Examples TodosCSS) ~ sub
    exampleLink (Contacts ContactsAll) ~ sub

  -- link "/query?key=value" lnk "Query Params"
  sub = pad (TRBL 10 10 10 40)

  exampleLink rt =
    route rt ~ selected rt . menuItem $
      text $
        routeTitle rt
  menuItem =
    pad (XY 20 10) . color White . hover (bg DarkHighlight)
  selected rt =
    if rt == current then bg DarkHighlight . border (L 2) . pad (L 18) else id

navigation :: AppRoute -> View c ()
navigation rt = do
  nav ~ bg Dark . color White . flexCol . showMenuHover $ do
    row $ do
      link [uri|https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|] "HYPERBOLE" ~ bold . pad 20 . logo . width 220
      space
      menuButton
    col ~ cls "menu" . onMobile (display None) . Cyber.font . Style.uppercase $ do
      exampleMenu rt
      space
      el ~ pad 10 . fontSize 12 $ do
        text "v"
        text $ cs $ showVersion version
 where
  menuButton =
    el ~ onDesktop (display None) . onMobile flexCol $ do
      el ~ pad 6 $ do
        el Icon.hamburger ~ color White . width 50 . height 50

  showMenuHover =
    css
      "show-menu"
      ".show-menu:hover > .menu"
      [ "display" :. "flex"
      ]

  -- https://www.fontspace.com/super-brigade-font-f96444
  logo =
    utility
      "logo"
      [ "background" :. "no-repeat center/90% url(/logo-robot.png)"
      , "color" :. "transparent"
      ]

onMobile :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onMobile = media (MaxWidth 650)

onDesktop :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onDesktop = media (MinWidth 650)

type Fragment = String

hackage :: Fragment -> Text -> View c ()
hackage uriFragment txt = do
  let docs = [uri|https://hackage-content.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|]
  link docs{uriFragment} @ att "target" "_blank" ~ Style.link $ do
    el ~ iconInline $ do
      Icon.bookOpen
      text txt
