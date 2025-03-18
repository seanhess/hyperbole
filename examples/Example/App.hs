{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Example.App where

import Control.Concurrent
  ( MVar
  , ThreadId
  , forkFinally
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Monad (forever, (>=>))
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Data.Version (showVersion)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.State.Static.Local
import Example.AppRoute
import Example.Cache (clientCache)
import Example.Colors
import Example.Effects.Debug as Debug
import Example.Effects.Random (GenRandom, runRandom)
import Example.Effects.Todos (Todos, runTodosSession)
import Example.Effects.Users as Users
import Example.Page.Autocomplete qualified as Autocomplete
import Example.Page.Concurrent qualified as Concurrent
import Example.Page.Contact qualified as Contact
import Example.Page.Contacts qualified as Contacts
import Example.Page.Counter qualified as Counter
import Example.Page.DataTable qualified as DataTable
import Example.Page.Errors qualified as Errors
import Example.Page.Filter qualified as Filter
import Example.Page.FormSimple qualified as FormSimple
import Example.Page.FormValidation qualified as FormValidation
import Example.Page.Javascript qualified as Javascript
import Example.Page.LazyLoading qualified as LazyLoading
import Example.Page.Redirects qualified as Redirects
import Example.Page.Requests qualified as Requests
import Example.Page.Sessions qualified as Sessions
import Example.Page.Simple qualified as Simple
import Example.Page.Todo qualified as Todo
import Example.Page.Transitions qualified as Transitions
import Example.Style qualified as Style
import Example.View.Layout as Layout (exampleLayout, examplesView)
import Foreign.Store (Store (..), lookupStore, readStore, storeAction, withStore)
import GHC.Generics (Generic)
import GHC.Word (Word32)
import Network.HTTP.Types (Header, Method, QueryItem, hCacheControl, methodPost, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static as Static (CacheContainer, CachingStrategy (..), Options (..), addBase)
import Network.Wai.Middleware.Static qualified as Static
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Paths_examples (version)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Web.Hyperbole
import Web.Hyperbole.Application (waiApp)
import Web.Hyperbole.Effect.Handler (RunHandlers)
import Web.Hyperbole.Effect.Server (Request (..))

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Examples on http://localhost:3000"
  users <- Users.initUsers
  count <- runEff $ runConcurrent Counter.initCounter
  cache <- clientCache
  Warp.run 3000 $
    Static.staticPolicyWithOptions cache (addBase "client/dist") $
      Static.staticPolicy (addBase "examples/static") $
        app users count

app :: UserStore -> TVar Int -> Application
app users count = do
  liveApp
    toDocument
    (runApp . routeRequest $ router)
 where
  runApp :: (Hyperbole :> es, IOE :> es) => Eff (GenRandom : Concurrent : Debug : Users : Todos : es) a -> Eff es a
  runApp = runTodosSession . runUsersIO users . runDebugIO . runConcurrent . runRandom

  router :: forall es. (Hyperbole :> es, Todos :> es, Users :> es, Debug :> es, Concurrent :> es, IOE :> es, GenRandom :> es) => AppRoute -> Eff es Response
  router (Hello h) = runPage $ hello h
  router (Contacts (Contact uid)) = Contact.response uid
  router (Contacts ContactsAll) = runPage Contacts.page
  router Concurrent = runPage Concurrent.page
  router Counter = runReader count $ runPage Counter.page
  router DataTable = runPage DataTable.page
  router Errors = runPage Errors.page
  router Examples = view Layout.examplesView
  router Filter = runPage Filter.page
  router FormSimple = runPage FormSimple.page
  router FormValidation = runPage FormValidation.page
  router LazyLoading = runPage LazyLoading.page
  router Autocomplete = runPage Autocomplete.page
  router Query = do
    p <- param "key"
    view $ el (pad 20) $ do
      text "key: "
      text p
  router RedirectNow = do
    redirect (routeUrl $ Hello Redirected)
  router Redirects = runPage Redirects.page
  router Requests = runPage Requests.page
  router Sessions = runPage Sessions.page
  router Simple = runPage Simple.page
  router Transitions = runPage Transitions.page
  router Todos = runPage Todo.page
  router Javascript = runPage Javascript.page
  router Main = do
    redirect (routeUrl Simple)

  -- Nested Router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Eff es (Page '[])
  hello (Greet who) = do
    pure $ exampleLayout (Hello $ Greet who) $ do
      row (gap 6 . pad 10) $ do
        el_ "Hello:"
        el_ $ text who
  hello Redirected = do
    pure $ exampleLayout RedirectNow $ el (pad 10) "You were redirected"

  -- Use the embedded version for real applications (see basicDocument).
  -- The link to /hyperbole.js here is just to make local development easier
  toDocument :: BL.ByteString -> BL.ByteString
  toDocument cnt =
    [i|<html>
      <head>
        <title>Hyperbole Examples</title>
        <meta httpEquiv="Content-Type" content="text/html" charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <script type="text/javascript" src="/hyperbole.js"></script>
        <script type="text/javascript" src="/custom.js"></script>
        <style type="text/css">#{cssResetEmbed}</style>
        <style type="text/css">body { background-color: \#d3dceb }</style>
      </head>
      <body>#{cnt}</body>
    </html>|]

{- | Made for local development
 -
 - ghcid --setup=Main.update --command="cabal repl exe:examples lib:hyperbole test" --run=Main.update --warnings
 -
 - Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    -- server is already running
    Just tidStore -> do
      restartAppInNewThread tidStore
 where
  -- callCommand "xmonadctl refreshFirefox"

  doneStore :: Store (MVar ())
  doneStore = Store 0

  -- shut the server down with killThread and wait for the done signal
  restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
  restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
    killThread tid
    withStore doneStore takeMVar
    readStore doneStore >>= start

  -- \| Start the server in a separate thread.
  start
    :: MVar ()
    -- \^ Written to when the thread is killed.
    -> IO ThreadId
  start done = do
    forkFinally
      Example.App.run
      -- Note that this implies concurrency
      -- between shutdownApp and the next app that is starting.
      -- Normally this should be fine
      (\_ -> putMVar done ())

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref

cacheMiddleware :: Application -> Application
cacheMiddleware = Wai.modifyResponse addCache
 where
  addCache = Wai.mapResponseHeaders ((hCacheControl, "private, max-age=60") :)
