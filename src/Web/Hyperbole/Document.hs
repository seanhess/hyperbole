{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Document where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Web.Hyperbole.View


data DocumentHead = DocumentHead
data Document = Document


{- | wrap HTML fragments in a simple document with a custom title and include required embeds

@
'liveApp' quickStartDocument ('routeRequest' router)
@

You must pass a function to Application that renders the entire document document function to import custom javascript, css, or add other information to the \<head\>

> import Data.String.Interpolate (i)
> import Web.Hyperbole (scriptEmbed, cssResetEmbed)
>
> #EMBED Example/Docs/App.hs customDocument
-}
quickStartDocument :: BL.ByteString -> BL.ByteString
quickStartDocument = document (mobileFriendly >> quickStart)


quickStart :: View DocumentHead ()
quickStart = do
  mobileFriendly
  style $ cs cssEmbed
  script' scriptEmbed
  script' scriptLiveReload


mobileFriendly :: View DocumentHead ()
mobileFriendly = do
  meta @ httpEquiv "Content-Type" . content "text/html" . charset "UTF-8"
  meta @ name "viewport" . content "width=device-width, initial-scale=1.0"


-- | Create a document from a document head and a body
document :: View DocumentHead () -> BL.ByteString -> BL.ByteString
document docHead cnt =
  [i|<html>
  <head>
    #{renderLazyByteString $ addContext DocumentHead docHead}
  </head>
  <body>
    #{cnt}
  </body>
</html>|]
