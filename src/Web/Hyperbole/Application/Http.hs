module Web.Hyperbole.Application.Http where

import Effectful
import Effectful.Concurrent
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (PendingConnection, defaultConnectionOptions)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> #EMBED Example/Docs/BasicPage.hs main
-}
liveApp :: Wai.Application
liveApp = do
  websocketsOr
    defaultConnectionOptions
    (runEff . runConcurrent . _)
    waiApp
 where
  waiApp :: Wai.Application
  waiApp req res = _
