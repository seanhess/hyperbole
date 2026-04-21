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

-- Haskell logo
-- https://commons.wikimedia.org/wiki/File:Haskell-Logo.svg
haskell :: View c ()
haskell = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" fill="currentColor" viewBox="0 0 17 12">
	<path d="M 0 12 L 4 6 L 0 0 L 3 0 L 7 6 L 3 12"/>
	<path d="M 4 12 L 8 6 L 4 0 L 7 0 L 15 12 L 12 12 L 9.5 8.25 L 7 12"/>
	<path d="M 13.66 8.5 L 12.333 6.5 L 17 6.5 L 17 8.5 M 11.666 5.5 L 10.333 3.5 L 17 3.5 L 17 5.5"/>
</svg>|]

-- GitHub logo
github :: View c ()
github = raw $ do
  [i|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor">
  <path d="M10.226 17.284c-2.965-.36-5.054-2.493-5.054-5.256 0-1.123.404-2.336 1.078-3.144-.292-.741-.247-2.314.09-2.965.898-.112 2.111.36 2.83 1.01.853-.269 1.752-.404 2.853-.404 1.1 0 1.999.135 2.807.382.696-.629 1.932-1.1 2.83-.988.315.606.36 2.179.067 2.942.72.854 1.101 2 1.101 3.167 0 2.763-2.089 4.852-5.098 5.234.763.494 1.28 1.572 1.28 2.807v2.336c0 .674.561 1.056 1.235.786 4.066-1.55 7.255-5.615 7.255-10.646C23.5 6.188 18.334 1 11.978 1 5.62 1 .5 6.188.5 12.545c0 4.986 3.167 9.12 7.435 10.669.606.225 1.19-.18 1.19-.786V20.63a2.9 2.9 0 0 1-1.078.224c-1.483 0-2.359-.808-2.987-2.313-.247-.607-.517-.966-1.034-1.033-.27-.023-.359-.135-.359-.27 0-.27.45-.471.898-.471.652 0 1.213.404 1.797 1.235.45.651.921.943 1.483.943.561 0 .92-.202 1.437-.719.382-.381.674-.718.944-.943"></path>
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
