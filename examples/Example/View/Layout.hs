{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import Control.Monad (when)
import Data.String.Conversions (cs)
import Data.Version (showVersion)
import Docs.Page
import Example.AppRoute
import Example.Colors (AppColor (..), cyan)
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Icon as Icon (hamburger)
import Paths_examples (version)
import Web.Atomic.CSS
import Web.Hyperbole

layout :: AppRoute -> View c () -> View c ()
layout rt = layout' (exampleMenu @() rt)

layoutSubnav :: forall sections c. (PageAnchor sections) => AppRoute -> View c () -> View c ()
layoutSubnav rt = layout' (exampleMenu @sections rt)

layout' :: View c () -> View c () -> View c ()
layout' menu contents =
  el ~ grow $ do
    navigation menu ~ position Fixed . zIndex 1 . onDesktop leftMenu . onMobile topMenu
    col ~ pad (TRBL 25 25 100 25) . gap 30 . onDesktop horizontal . onMobile vertical $ do
      contents
 where
  leftMenu = width menuWidth . left 0 . top 0 . bottom 0
  horizontal = margin (L menuWidth)
  vertical = margin (T menuHeight)

  topMenu = top 0 . right 0 . left 0

  menuWidth = 230
  menuHeight = 70

-- Navigation --------------------------------------

navigation :: View c () -> View c ()
navigation menu = do
  nav ~ bg Dark . color White . flexCol . showMenuHover $ do
    row $ do
      link [uri|https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|] "HYPERBOLE" ~ bold . pad 20 . logo . width 220
      space
      menuButton
    col ~ cls "menu" . onMobile (display None) . Cyber.font . Style.uppercase $ do
      menu
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

-- Menu --------------------------------------

exampleMenu :: forall sections c. (PageAnchor sections) => AppRoute -> View c ()
exampleMenu current = do
  col ~ color White $ do
    exampleLink Intro
    exampleLink Basics
    exampleLink CSS
    exampleLink SideEffects
    exampleLink State
    exampleLink Concurrency
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
  sub :: (Styleable h) => CSS h -> CSS h
  sub = pad (TRBL 5 10 5 40) . fontSize 14

  menuItem :: (Styleable h) => CSS h -> CSS h
  menuItem =
    pad (XY 20 10) . hover (bg DarkHighlight)

  selected rt =
    if rt == current then bg DarkHighlight . border (L 4) . pad (L 16) . color cyan else id

  exampleLink rt = do
    route rt ~ selected rt . menuItem $
      text $
        routeTitle
          rt
    when (rt == current) $ do
      mapM_ anchorLink (subnav @sections)

  anchorLink :: (PageAnchor a) => a -> View c ()
  anchorLink a = do
    tag "a" ~ sub . menuItem @ att "href" ("#" <> pageAnchor a) $ do
      text $ navEntry a

onMobile :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onMobile = media (MaxWidth 650)

onDesktop :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onDesktop = media (MinWidth 650)
