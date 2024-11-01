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

import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.Async
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Network.HTTP.Types (parseQuery)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server (Host (..), Request (..), Response (..), Server, SocketError (..), runServerSockets, runServerWai)
import Web.Hyperbole.Embed (cssResetEmbed, scriptEmbed)
import Web.Hyperbole.Route


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> main = do
>   run 3000 $ do
>   liveApp (basicDocument "Example") $ do
>      page mainPage
-}
liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, Concurrent, IOE] Response -> Wai.Application
liveApp toDoc app =
  websocketsOr
    defaultConnectionOptions
    (runEff . runConcurrent . socketApp app)
    (waiApp toDoc app)


waiApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, Concurrent, IOE] Response -> Wai.Application
waiApp toDoc actions req res = do
  rr <- runEff $ runConcurrent $ runServerWai toDoc req res $ runHyperbole actions
  case rr of
    Nothing -> error "Missing required response in handler"
    Just r -> pure r


socketApp :: (IOE :> es, Concurrent :> es) => Eff (Hyperbole : Server : es) Response -> PendingConnection -> Eff es ()
socketApp actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  forever $ do
    ereq <- runErrorNoCallStack @SocketError $ receiveRequest conn
    case ereq of
      Left e -> liftIO $ putStrLn $ "SOCKET ERROR " <> show e
      Right r -> do
        a <- async (runServerSockets conn r $ runHyperbole actions)
        -- throw exceptions in this thread
        link a
        pure ()
 where
  receiveRequest :: (IOE :> es, Error SocketError :> es) => Connection -> Eff es Request
  receiveRequest conn = do
    t <- receiveText conn
    case parseMessage t of
      Left e -> throwError e
      Right r -> pure r

  receiveText :: (IOE :> es) => Connection -> Eff es Text
  receiveText conn = do
    -- c <- ask @Connection
    liftIO $ WS.receiveData conn

  parseMessage :: Text -> Either SocketError Request
  parseMessage t = do
    case T.splitOn "\n" t of
      [url, host, cook, body] -> parse url cook host (Just body)
      [url, host, cook] -> parse url cook host Nothing
      _ -> Left $ InvalidMessage t
   where
    parseUrl :: Text -> Either SocketError (Text, Text)
    parseUrl u =
      case T.splitOn "?" u of
        [url, query] -> pure (url, query)
        _ -> Left $ InvalidMessage u

    parse :: Text -> Text -> Text -> Maybe Text -> Either SocketError Request
    parse url cook hst mbody = do
      (u, q) <- parseUrl url
      let path = paths u
          query = parseQuery (cs q)
          cookies = parseCookies $ cs $ header cook
          host = Host $ cs $ header hst
          method = "POST"
          body = cs $ fromMaybe "" mbody
      pure $ Request{path, host, query, body, method, cookies}

    paths p = filter (/= "") $ T.splitOn "/" p

    -- drop up to the colon, then ': '
    header = T.drop 2 . T.dropWhile (/= ':')


{- | wrap HTML fragments in a simple document with a custom title and include required embeds

@
'liveApp' (basicDocument "App Title") ('routeRequest' router)
@

You may want to specify a custom document function instead:

> myDocument :: ByteString -> ByteString
> myDocument content =
>   [i|<html>
>     <head>
>       <title>#{title}</title>
>       <script type="text/javascript">#{scriptEmbed}</script>
>       <style type="text/css">#{cssResetEmbed}</style>
>     </head>
>     <body>#{content}</body>
>   </html>|]
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
import Page.Messages qualified as Messages
import Page.Users qualified as Users

data AppRoute
  = Main
  | Messages
  | Users UserId
  deriving (Eq, Generic, 'Route')

router :: ('Hyperbole' :> es) => AppRoute -> 'Eff' es 'Response'
router Messages = 'page' Messages.page
router (Users uid) = 'page' $ Users.page uid
router Main = do
  'view' $ do
    'el_' "click a link below to visit a page"
    'route' Messages id \"Messages\"

main = do
  'run' 3000 $ do
    'liveApp' ('basicDocument' \"Example\") (routeRequest router)
@
-}
routeRequest :: (Hyperbole :> es, Route route) => (route -> Eff es Response) -> Eff es Response
routeRequest actions = do
  path <- reqPath
  case findRoute path of
    Nothing -> send $ RespondEarly NotFound
    Just rt -> actions rt
