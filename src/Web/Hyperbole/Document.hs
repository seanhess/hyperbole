module Web.Hyperbole.Document where

import Data.String.Conversions (cs)
import Web.Hyperbole.View


data DocumentHead = DocumentHead
data Body = Body


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
quickStartDocument :: View DocumentHead ()
quickStartDocument = do
  title "Hyperbole"
  mobileFriendly
  style $ cs cssResetEmbed
  script' scriptEmbed
  script' scriptLiveReload


mobileFriendly :: View DocumentHead ()
mobileFriendly = do
  meta @ httpEquiv "Content-Type" . content "text/html" . charset "UTF-8"
  meta @ name "viewport" . content "width=device-width, initial-scale=1.0"
