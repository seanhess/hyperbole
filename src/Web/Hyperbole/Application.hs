{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Application
  ( waiApp
  , websocketsOr
  , defaultConnectionOptions
  , liveApp
  , socketApp
  , basicDocument
  , routeRequest
  ) where

import Control.Monad (forever, void)
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Dispatch.Dynamic
import Effectful.Exception (SomeException, trySync)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (reqPath)
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Effect.Server.Socket (runServerSockets)
import Web.Hyperbole.Effect.Server.Socket qualified as Socket
import Web.Hyperbole.Effect.Server.Types
import Web.Hyperbole.Effect.Server.Wai (runServerWai)
import Web.Hyperbole.Route
import Web.Hyperbole.View.Embed (cssResetEmbed, scriptEmbed)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, Concurrent, IOE] Response -> Wai.Application
liveApp toDoc app req res = do
  websocketsOr
    defaultConnectionOptions
    (runEff . runConcurrent . socketApp app)
    (waiApp toDoc app)
    req
    res


waiApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, Concurrent, IOE] Response -> Wai.Application
waiApp toDoc actions req res = do
  rr <- runEff $ runConcurrent $ runServerWai toDoc req res $ do
    req' <- send LoadRequest
    runHyperbole req' actions
  case rr of
    Nothing -> error "Missing required response in handler"
    Just r -> pure r


socketApp :: (IOE :> es, Concurrent :> es) => Eff (Hyperbole : Server : es) Response -> PendingConnection -> Eff es ()
socketApp actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  forever $ do
    runServerSockets conn $ do
      -- block loading the request
      req <- send LoadRequest

      -- run handler in the background = async
      void $ async $ do
        res <- trySync $ runHyperbole req actions
        case res of
          -- TODO: catch socket errors separately from SomeException?
          Left (ex :: SomeException) -> do
            -- It's not safe to send any exception over the wire
            -- log it to the console and send the error to the client
            liftIO $ print ex
            res2 <- trySync $ Socket.sendError req.requestId conn (serverError "Internal Server Error")
            case res2 of
              Left e -> liftIO $ putStrLn $ "Socket Error while sending previous error to client: " <> show e
              Right _ -> pure ()
          Right _ -> pure ()


{- | wrap HTML fragments in a simple document with a custom title and include required embeds

@
'liveApp' (basicDocument "App Title") ('routeRequest' router)
@

You may want to specify a custom document function to import custom javascript, css, or add other information to the \<head\>

> import Data.String.Interpolate (i)
> import Web.Hyperbole (scriptEmbed, cssResetEmbed)
>
> #EMBED Example/Docs/App.hs customDocument
-}
basicDocument :: Text -> BL.ByteString -> BL.ByteString
basicDocument title cnt =
  [i|<html>
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>
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
