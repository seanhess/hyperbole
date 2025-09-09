module Web.Hyperbole.Application
  ( waiApp
  , websocketsOr
  , defaultConnectionOptions
  , liveApp
  , liveAppWith
  , ServerOptions (..)
  , defaultErrorMessage
  , defaultError
  , socketApp
  , quickStartDocument
  , routeRequest
  ) where

import Control.Exception
import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Concurrent.Async
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ConnectionException (..), PendingConnection, defaultConnectionOptions, withPingThread)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Document
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (reqPath)
import Web.Hyperbole.Effect.Response (notFound)
import Web.Hyperbole.Route
import Web.Hyperbole.Server.Options
import Web.Hyperbole.Server.Socket (handleRequestSocket)
import Web.Hyperbole.Server.Wai (handleRequestWai)
import Web.Hyperbole.Types.Response


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
liveApp doc =
  liveAppWith $
    ServerOptions
      { toDocument = doc
      , serverError = defaultError
      }


-- | Run a Hyperbole application, customizing both the document and the format of server errors
liveAppWith :: ServerOptions -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
liveAppWith opts app req =
  websocketsOr
    defaultConnectionOptions
    (\pend -> socketApp opts req app pend `catch` suppressMessages)
    (waiApp opts app)
    req


waiApp :: ServerOptions -> Eff '[Hyperbole, Concurrent, IOE] Response -> Wai.Application
waiApp opts actions req res = do
  runEff $ runConcurrent $ handleRequestWai opts req res actions


socketApp :: ServerOptions -> Wai.Request -> Eff '[Hyperbole, Concurrent, IOE] Response -> PendingConnection -> IO ()
socketApp opts req actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  -- ping to keep the socket alive
  withPingThread conn 25 (pure ()) $ do
    forever $ do
      runEff $ runConcurrent $ handleRequestSocket opts req conn actions


suppressMessages :: ConnectionException -> IO a
suppressMessages ex = do
  -- The default version of Network.Websockets prints out CloseRequest and ConnectionClosed errors
  -- it's like they're using these as events instead of exceptions
  case ex of
    ConnectionClosed -> do
      -- putStrLn "CAUGHT ConnectionClosed"
      pure undefined
    CloseRequest _cd _msg -> do
      -- putStrLn "CAUGHT CloseRequest"
      pure undefined
    other -> throwIO other


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
