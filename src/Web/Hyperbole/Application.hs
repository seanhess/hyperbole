{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Application
  ( waiApp
  , websocketsOr
  , defaultConnectionOptions
  , liveApp
  , socketApp
  , quickStartDocument
  , routeRequest
  ) where

import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Dispatch.Dynamic
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (reqPath)
import Web.Hyperbole.Route
import Web.Hyperbole.Server.Socket (handleRequestSocket)
import Web.Hyperbole.Server.Wai (handleRequestWai)
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View.Embed (cssResetEmbed, scriptEmbed, scriptLiveReload)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
liveApp toDoc app req res = do
  websocketsOr
    defaultConnectionOptions
    (socketApp app)
    (waiApp toDoc app)
    req
    res


waiApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
waiApp toDoc actions req res = do
  runEff $ runConcurrent $ handleRequestWai toDoc req res actions


socketApp :: Eff '[Hyperbole, Concurrent, IOE] Response -> PendingConnection -> IO ()
socketApp actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  forever $ do
    runEff $ runConcurrent $ handleRequestSocket conn actions


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


{- | Route URL patterns to different pages


@
#EMBED Example/Docs/App.hs import Example.Docs.Page

#EMBED Example/Docs/App.hs type UserId

#EMBED Example/Docs/App.hs data AppRoute

#EMBED Example/Docs/App.hs instance Route

#EMBED Example/Docs/App.hs router
@
-}
routeRequest :: (Hyperbole :> es, Route route) => (route -> Eff es Response) -> Eff es Response
routeRequest actions = do
  pth <- reqPath
  case findRoute pth.segments of
    Nothing -> send $ RespondNow NotFound
    Just rt -> actions rt
