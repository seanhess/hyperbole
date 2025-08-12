{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Application.Http where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Network.HTTP.Types (Status, status200, status303, status400, status500)
import Network.Wai (ResponseReceived, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions)
import Web.Hyperbole.Data.URI (uriToText)
import Web.Hyperbole.Effect.Page
import Web.Hyperbole.Effect.Server.Wai (ContentType (..), contentType, runPageWai)
import Web.Hyperbole.View (el, renderLazyByteString)

import Web.Hyperbole.HyperView (Root (..))
import Web.Hyperbole.Page (PageView)
import Web.Hyperbole.View (addContext)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: Eff '[Page, Error Interrupt, IOE] PageResponse -> Wai.Application
liveApp handlePage = do
  websocketsOr
    defaultConnectionOptions
    (runEff . runConcurrent . noop)
    (waiApp handlePage)
 where
  noop = const (pure ())


waiApp :: Eff '[Page, Error Interrupt, IOE] PageResponse -> Wai.Application
waiApp handlePage req respond = do
  wr <- runEff $ runErrorNoCallStack @Interrupt $ runPageWai req handlePage
  case wr of
    -- TODO:
    Left int -> respond $ interruptResponse int
    Right (pr, clnt) -> pageResponse clnt pr
 where
  interruptResponse :: Interrupt -> Wai.Response
  interruptResponse = \case
    NotFound ->
      Wai.responseLBS status400 [contentType ContentText] "Not Found"
    Redirect u -> do
      let uri = cs $ uriToText u
          heads = [("Location", uri), contentType ContentHtml]
      Wai.responseLBS status303 heads $ "Redirecting: " <> cs uri
    Err err ->
      errorResponse err

  errorResponse :: PageError -> Wai.Response
  errorResponse = \case
    other ->
      Wai.responseLBS status500 [contentType ContentText] $ cs (show other)

  pageResponse :: Client -> PageResponse -> IO ResponseReceived
  pageResponse clnt pr = do
    let body = renderLazyByteString pr.view
    respond $ Wai.responseLBS status200 [contentType ContentHtml] body


exampleApp :: Wai.Application
exampleApp = liveApp handlePage
 where
  handlePage = do
    p <- examplePage
    pure $ PageResponse $ addContext Root p


examplePage :: Eff es (PageView '[])
examplePage = pure $ el "Hello!"


main :: IO ()
main = do
  putStrLn "Starting.."
  Warp.run 3000 exampleApp
