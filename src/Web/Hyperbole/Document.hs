{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Document where

import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import GHC.Generics (Generic)
import Web.Hyperbole.View


data Document = Document


{- | 'liveApp' requires a function which turns an html fragment into an entire html document. Use this to import javascript, css, etc. Use 'quickStartDocument' to get going quickly

> #EMBED Example/Docs/App.hs app
-}
document :: View DocumentHead () -> BL.ByteString -> BL.ByteString
document docHead cnt =
  [i|<!doctype html>
  <html>
  <head>
    <meta charset="UTF-8"/>
    #{renderLazyByteString $ runViewContext DocumentHead () docHead}
  </head>
  <body>
    #{cnt}
  </body>
</html>|]


{- | Create a custom \<head\> to use with 'document'. Remember to include at least `scriptEmbed`!

> import Web.Hyperbole (scriptEmbed, cssEmbed)
>
> #EMBED Example/Docs/App.hs documentHead
>
> #EMBED Example/Docs/App.hs app
-}
data DocumentHead = DocumentHead
  deriving (Generic, ViewId)


{- | A simple mobile-friendly document with all required embeds and live reload

@
'liveApp' quickStartDocument ('routeRequest' router)
@
-}
quickStartDocument :: BL.ByteString -> BL.ByteString
quickStartDocument = document (mobileFriendly >> quickStart)


-- | A simple mobile-friendly header with all required embeds and live reload
quickStart :: View DocumentHead ()
quickStart = do
  mobileFriendly
  style cssEmbed
  script' scriptEmbed
  script' scriptLiveReload


-- | Set the viewport to handle mobile zoom
mobileFriendly :: View DocumentHead ()
mobileFriendly = do
  meta @ name "viewport" . content "width=device-width, initial-scale=1.0"
