{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import App.Docs (PageAnchor (..))
import App.Route
import Data.String.Conversions (cs)
import Data.Version (showVersion)
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Icon as Icon (github, hamburger, haskell)
import Example.View.Menu (menu, nextPage)
import Paths_demo (version)
import Web.Atomic.CSS
import Web.Hyperbole

layout :: AppRoute -> View c () -> View c ()
layout rt = layout' rt (menu @() rt)

layoutSubnav :: forall sections c. (PageAnchor sections) => AppRoute -> View c () -> View c ()
layoutSubnav rt = layout' rt (menu @sections rt)

layout' :: AppRoute -> View c () -> View c () -> View c ()
layout' rt chosenMenu contents =
  el ~ grow $ do
    navigation chosenMenu ~ position Fixed . zIndex 1 . onDesktop leftMenu . onMobile topMenu
    col ~ pad (TRBL 25 25 100 25) . gap 30 . onDesktop horizontal . onMobile vertical $ do
      contents
      case nextPage rt of
        Nothing -> none
        Just nxt -> do
          el $ do
            text "▶️ Continue to "
            link (routeUri nxt) ~ Style.link $ text $ routeTitle nxt
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
      link hackageUrl "HYPERBOLE" ~ bold . pad 20 . logo . width 220
      space
      menuButton
    col ~ cls "menu" . onMobile (display None) . Cyber.font . Style.uppercase $ do
      chosenMenu
      space
      row ~ pad (TL 20 10) . gap 10 . utility "items-center" ["align-items" :. "center"] $ do
        el ~ fontSize 12 $ do
          text "v"
          text $ cs $ showVersion version
        row $ do
          link hackageUrl (el ~ width 20 . height 20 . flexRow . utility "items-center" ["align-items" :. "center"] $ Icon.haskell) ~ pad 8 . hover (bg DarkHighlight)
          link githubUrl (el ~ width 20 . height 20 . flexRow . utility "items-center" ["align-items" :. "center"] $ Icon.github) ~ pad 8 . hover (bg DarkHighlight)
 where
  hackageUrl = [uri|https://hackage.haskell.org/package/hyperbole|]
  githubUrl = [uri|https://github.com/seanhess/hyperbole|]

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
