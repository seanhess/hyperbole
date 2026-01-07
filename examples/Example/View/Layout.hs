{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import App.Route
import Control.Monad (when)
import Data.String.Conversions (cs)
import Data.Version (showVersion)
import Docs.Page
import Example.Colors (AppColor (..), cyan)
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Icon as Icon (hamburger)
import Paths_examples (version)
import Web.Atomic.CSS
import Web.Hyperbole

layout :: AppRoute -> View c () -> View c ()
layout rt = layout' (menu @() rt)

layoutSubnav :: forall sections c. (PageAnchor sections) => AppRoute -> View c () -> View c ()
layoutSubnav rt = layout' (menu @sections rt)

layout' :: View c () -> View c () -> View c ()
layout' chosenMenu contents =
  el ~ grow $ do
    navigation chosenMenu ~ position Fixed . zIndex 1 . onDesktop leftMenu . onMobile topMenu
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
navigation chosenMenu = do
  nav ~ bg Dark . color White . flexCol . showMenuHover $ do
    row $ do
      link [uri|https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html|] "HYPERBOLE" ~ bold . pad 20 . logo . width 220
      space
      menuButton
    col ~ cls "menu" . onMobile (display None) . Cyber.font . Style.uppercase $ do
      chosenMenu
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

menu :: forall sections c. (PageAnchor sections) => AppRoute -> View c ()
menu current = do
  col ~ color White $ do
    docLink Intro
    docLink Basics
    docLink CSS
    docLink SideEffects
    docLink State
    docLink Concurrency
    docLink Requests
    docLink (Data DataLists)
    case current of
      Data _ -> do
        docLink (Data SortableTable) ~ sub
        docLink (Data Autocomplete) ~ sub
        docLink (Data Filter) ~ sub
        docLink (Data LoadMore) ~ sub
      _ -> none
    docLink (Forms FormSimple)
    docLink Interactivity
    docLink Errors
    docLink OAuth2
    docLink Javascript
    docLink Advanced
    docLink (Examples BigExamples)
    case current of
      Examples _ ->
        completeExamples
      (Contacts _) ->
        completeExamples
      _ -> none
 where
  completeExamples = do
    docLink (Examples Todos) ~ sub
    docLink (Examples TodosCSS) ~ sub
    docLink (Contacts ContactsAll) ~ sub

  -- link "/query?key=value" lnk "Query Params"
  sub :: (Styleable h) => CSS h -> CSS h
  sub = pad (TRBL 5 10 5 40) . fontSize 14

  menuItem :: (Styleable h) => CSS h -> CSS h
  menuItem =
    pad (XY 20 10) . hover (bg DarkHighlight)

  selected rt =
    if rt == current then bg DarkHighlight . border (L 4) . pad (L 16) . color cyan else id

  docLink rt = do
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
