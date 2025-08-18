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
import Effectful
import Effectful.Concurrent.Async
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions, withPingThread)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Document
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (reqPath)
import Web.Hyperbole.Effect.Response (notFound)
import Web.Hyperbole.Route
import Web.Hyperbole.Server.Socket (handleRequestSocket)
import Web.Hyperbole.Server.Wai (handleRequestWai)
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: View DocumentHead () -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
liveApp doc app req res = do
  websocketsOr
    defaultConnectionOptions
    (socketApp req app)
    (waiApp doc app)
    req
    res


waiApp :: View DocumentHead () -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
waiApp doc actions req res = do
  runEff $ runConcurrent $ handleRequestWai doc req res actions


socketApp :: Wai.Request -> Eff '[Hyperbole, Concurrent, IOE] Response -> PendingConnection -> IO ()
socketApp req actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  -- ping to keep the socket alive
  withPingThread conn 25 (pure ()) $ do
    forever $ do
      runEff $ runConcurrent $ handleRequestSocket req conn actions


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
  maybe notFound actions $ findRoute pth.segments
