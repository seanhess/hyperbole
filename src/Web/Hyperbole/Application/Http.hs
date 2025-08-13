{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Application.Http where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Network.HTTP.Types (Header, HeaderName, Status, status200, status303, status400, status500)
import Network.Wai (ResponseReceived, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions)
import Web.Hyperbole.Application.Socket qualified as Socket
import Web.Hyperbole.Data.Cookie (Cookie)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData
import Web.Hyperbole.Data.URI (path, uriToText)
import Web.Hyperbole.Effect.Handler (RunHandlers, runHandlers)
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.Effect.Page
import Web.Hyperbole.Effect.Server.Wai (ContentType (..), contentType, runPageWai)
import Web.Hyperbole.Route
import Web.Hyperbole.Types.Event
import Web.Hyperbole.View (el, renderLazyByteString)

import Data.String.Interpolate (i)
import Effectful.Reader.Static
import GHC.Generics (Generic)
import Web.Atomic.CSS hiding (style)
import Web.Hyperbole.Application.Document
import Web.Hyperbole.Effect.Query (setParam)
import Web.Hyperbole.Effect.Session (Session (..), saveSession)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Page (PageView)
import Web.Hyperbole.Types.ViewAction
import Web.Hyperbole.Types.ViewId
import Web.Hyperbole.View


data PageEndpoint es = PageEndpoint
  { load :: Eff es DocumentBody
  , update :: Event TargetViewId Text -> Eff es (View () ())
  }


liftRunEndpoint :: (forall a. Eff es1 a -> Eff es2 a) -> Eff es1 (PageEndpoint es1) -> Eff es2 (PageEndpoint es2)
liftRunEndpoint run eff = do
  PageEndpoint l u <- run eff
  pure $ PageEndpoint (run l) (run . u)


runPage :: forall views es. (Page :> es, RunHandlers views es) => Eff es (PageView views) -> PageEndpoint es
runPage pg =
  PageEndpoint
    { load = runPageLoad pg
    , update = handleUpdate
    }
 where
  handleUpdate :: Event TargetViewId Text -> Eff es (View () ())
  handleUpdate ev = do
    -- we need enough info to run hyperbole here!
    mview <- runHyperbole $ runHandlers @views ev
    maybe notFound pure mview

  runHyperbole :: Eff (Hyperbole : es) a -> Eff es a
  runHyperbole = undefined


type RunPage es a = (forall views. Eff es (PageView views) -> Eff es a)


type Router route es = (forall a. RunPage es a -> route -> Eff es a)


-- expects a router function
routeRequest :: (Page :> es, Route route) => (route -> Eff es a) -> Eff es a
routeRequest actions = do
  pth <- (.path) <$> pageInfo
  maybe notFound actions $ findRoute pth.segments


-- loadRoute :: (Route r, Page :> es) => DocumentHead -> Router r es -> Eff es Document
-- loadRoute docHead router = routeRequest $ router (runDocument docHead)

-- runDocument :: (Page :> es) => DocumentHead -> Eff es (View (Root views) ()) -> Eff es Document
-- runDocument docHead pageRoot = do
--   root <- pageRoot
--   -- myHead is configuration...
--   pure $ Document docHead $ addContext Root root

runPageLoad :: (Page :> es) => Eff es (View (Root views) ()) -> Eff es DocumentBody
runPageLoad pageRoot = do
  root <- pageRoot
  pure $ DocumentBody $ addContext Root root


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: DocumentHead -> (es ~ '[Page, Error Interrupt, IOE]) => Eff es (PageEndpoint es) -> Wai.Application
liveApp docHead pages req = do
  websocketsOr
    defaultConnectionOptions
    (runEff . runConcurrent . connectSock)
    (waiApp docHead handleLoad)
    req
 where
  connectSock = Socket.socketApp req

  handleLoad :: Eff '[Page, Error Interrupt, IOE] DocumentBody
  handleLoad = do
    end <- pages
    end.load


waiApp :: DocumentHead -> Eff '[Page, Error Interrupt, IOE] DocumentBody -> Wai.Application
waiApp docHead handlePage req respond = do
  wr <- runEff $ runErrorNoCallStack @Interrupt $ runPageWai req handlePage
  case wr of
    Left int -> respond $ interruptResponse int
    Right (pr, clnt) -> pageResponse clnt pr
 where
  interruptResponse :: Interrupt -> Wai.Response
  interruptResponse = \case
    NotFound ->
      Wai.responseLBS status400 [contentType ContentText] "Not Found"
    Redirect u -> do
      let loc = cs $ uriToText u
          heads = [("Location", loc), contentType ContentHtml]
      Wai.responseLBS status303 heads $ "Redirecting: " <> cs loc
    Err err ->
      errorResponse err

  errorResponse :: PageError -> Wai.Response
  errorResponse = \case
    other ->
      -- TODO: error handling copy from Application
      Wai.responseLBS status500 [contentType ContentText] $ cs (show other)

  pageResponse :: Client -> DocumentBody -> IO ResponseReceived
  pageResponse clnt docBody = do
    let body = renderDocument $ mapHead (setQuery clnt.query) (Document docHead docBody)
        heads = contentType ContentHtml : responseHeaders clnt
    respond $ Wai.responseLBS status200 heads body


setQuery :: QueryData -> DocumentHead -> DocumentHead
setQuery q (DocumentHead hd) =
  case q of
    QueryData [] -> DocumentHead hd
    _ ->
      let qout = Aeson.encode q
       in DocumentHead $ hd <> [i|<script type="text/javascript">window.hypQuery = #{qout}</script>|]


responseHeaders :: Client -> [Header]
responseHeaders clnt =
  setCookies
 where
  setCookies =
    fmap setCookie $ Cookie.toList clnt.session

  setCookie :: Cookie -> (HeaderName, BS.ByteString)
  setCookie cookie =
    ("Set-Cookie", Cookie.render cookie)


----------------------------------------------------------------

-- Apply document in runner? no need to pass it in globally

myHead :: DocumentHead
myHead = documentHead $ do
  title "Hyperbole"
  meta @ httpEquiv "Content-Type" . content "text/html" . charset "UTF-8"
  meta @ name "viewport" . content "width=device-width, initial-scale=1.0"
  style cssResetEmbed
  script' scriptEmbed
  script' scriptLiveReload


----------------------------------------------------------------

data AppRoute
  = Main
  | Other
  deriving (Generic, Eq, Route)


-- myRouter :: (Page :> es, Reader Int :> es) => RunPage es a -> AppRoute -> Eff es a
-- myRouter run = \case
--   Main -> run counterPage
--   Other -> run $ runReader @Int 5 otherPage

exampleApp :: Wai.Application
exampleApp = liveApp myHead load
 where
  load :: (es ~ [Page, Error Interrupt, IOE]) => Eff es (PageEndpoint es)
  load = do
    liftRunEndpoint runApp $ routeRequest myRouter

  runApp :: Eff (Reader Int : es) a -> Eff es a
  runApp = runReader @Int 3

  -- runAppEndpoint :: Eff (Reader Int : es) (PageEndpoint (Reader Int : es)) -> Eff es (PageEndpoint es)
  -- runAppEndpoint eff = do

  myRouter :: (Page :> es, Reader Int :> es) => AppRoute -> Eff es (PageEndpoint es)
  myRouter = \case
    Main -> pure $ runPage counterPage
    Other -> pure $ runPage $ runReader @Int 5 otherPage


data Prefs = Prefs
  { message :: Text
  }
  deriving (Generic, Aeson.ToJSON, Aeson.FromJSON, Session)


otherPage :: (Page :> es, Reader Int :> es) => Eff es (PageView '[])
otherPage = do
  n <- ask @Int
  pure $ do
    el "Other"
    el $ text $ cs $ show n


counterPage :: (Page :> es, Reader Int :> es) => Eff es (PageView '[Counter])
counterPage = do
  setParam @Text "woot" "hello"
  saveSession $ Prefs "Wahoo"
  n <- ask @Int
  pure $ col ~ pad 20 $ hyper Counter (viewCount n)


data Counter = Counter
  deriving (Generic, ViewId)


instance HyperView Counter es where
  data Action Counter
    = Increment Int
    | Decrement Int
    deriving (Generic, ViewAction)


  update (Increment n) = do
    pure $ viewCount (n + 1)
  update (Decrement n) = do
    pure $ viewCount (n - 1)


viewCount :: Int -> View Counter ()
viewCount n = col ~ gap 10 $ do
  row $ do
    el ~ bold . fontSize 48 . border 1 . pad (XY 20 0) $ text $ cs $ show n
  row ~ gap 10 $ do
    button (Decrement n) "Decrement" ~ border 1 . pad 5 . hover (bg (HexColor "#FF0"))
    button (Increment n) "Increment" ~ border 1 . pad 5 . hover (bg (HexColor "#FF0"))


main :: IO ()
main = do
  putStrLn "Starting.."
  Warp.run 3000 exampleApp
