{-# LANGUAGE QuasiQuotes #-}

module Example.View.Icon where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole.View

hamburger :: View c ()
hamburger =
  raw
    [i|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="size-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5" />
</svg>|]

xCircle :: View c ()
xCircle = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="w-6 h-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="M9.75 9.75l4.5 4.5m0-4.5l-4.5 4.5M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
</svg>|]

checkCircle :: View c ()
checkCircle = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="size-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="M9 12.75 11.25 15 15 9.75M21 12a9 9 0 1 1-18 0 9 9 0 0 1 18 0Z" />
</svg>|]

check :: View c ()
check = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="size-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="m4.5 12.75 6 6 9-13.5" />
</svg>|]

chevronDown :: View c ()
chevronDown = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="size-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="m19.5 8.25-7.5 7.5-7.5-7.5" />
</svg>|]

-- see icons.svg
icon :: Text -> View c ()
icon iconId = tag "svg" ~ icn $ do
  tag "use" @ att "href" ("/icons.svg#" <> iconId) $ none
 where
  icn =
    utility
      "icn"
      [ "width" :. "1.2em"
      , "height" :. "1.2em"
      , "display" :. "inline-block"
      , "fill" :. "none"
      , "stroke" :. "current-color"
      , "transform" :. "translateY(0.175em)"
      ]

bookOpen :: View c ()
bookOpen = icon "book"

linkOut :: View c ()
linkOut = icon "link-out"

iconInline :: (Styleable h) => CSS h -> CSS h
iconInline = flexRow . gap 2 . utility "items-baseline" ["align-items" :. "baseline"]
