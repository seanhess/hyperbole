{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Application.Document where

import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Web.Hyperbole.View (View, renderLazyByteString, renderText, tag)
import Web.Hyperbole.View.Embed (cssResetEmbed, scriptEmbed, scriptLiveReload)


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
quickStartDocument cnt =
  [i|<html>
      <head>
        <title>Hyperbole</title>
        <meta httpEquiv="Content-Type" content="text/html" charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <script type="text/javascript">#{scriptEmbed}</script>
        <script type="text/javascript">#{scriptLiveReload}</script>
        <style type="text/css">#{cssResetEmbed}</style>
      </head>
      <body>#{cnt}</body>
  </html>|]


newtype DocumentHead = DocumentHead {html :: BL.ByteString}


documentHead :: View () () -> DocumentHead
documentHead vh = do
  DocumentHead $ renderLazyByteString vh


data Document = Document
  { head :: DocumentHead
  , body :: View () ()
  }


renderDocument :: Document -> BL.ByteString
renderDocument doc =
  let hd = doc.head.html
      bd = renderLazyByteString doc.body
   in [i|<html>
      <head>
        #{hd}
      </head>
      <body>
        #{bd}
      </body>
  </html>|]


mapHead :: (DocumentHead -> DocumentHead) -> Document -> Document
mapHead f doc = Document{head = f doc.head, body = doc.body}
