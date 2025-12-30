{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe (fromMaybe)
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
import Effectful.Environment (runEnvironment)
import Effectful.Reader.Dynamic
import Effectful.State.Static.Local
import Example.AppRoute as Route
import Example.Cache (clientCache)
import Example.Colors
import Example.Config
import Example.Effects.Debug as Debug
import Example.Effects.Todos (Todos, runTodosSession)
import Example.Effects.Users as Users
import Example.Page.Advanced qualified as Advanced
import Example.Page.CSS qualified as CSS
import Example.Page.Chat qualified as Chat
import Example.Page.Concurrency qualified as Concurrency
import Example.Page.Contact qualified as Contact
import Example.Page.Contacts qualified as Contacts
import Example.Page.Counter qualified as Counter
import Example.Page.DataLists.Autocomplete qualified as Autocomplete
import Example.Page.DataLists.DataTable qualified as DataTable
import Example.Page.DataLists.Filter qualified as Filter
import Example.Page.DataLists.LoadMore qualified as LoadMore
import Example.Page.Errors qualified as Errors
import Example.Page.Forms qualified as Forms
import Example.Page.Interactivity qualified as Interactivity
import Example.Page.Intro qualified as Intro
import Example.Page.Javascript qualified as Javascript
import Example.Page.OAuth2 qualified as OAuth2
import Example.Page.Requests qualified as Requests
import Example.Page.State.Actions qualified as Actions
import Example.Page.State.Effects qualified as Effects
import Example.Page.State.Query qualified as Query
import Example.Page.State.Sessions qualified as Sessions
import Example.Page.Test qualified as Test
import Example.Page.Todos.Todo qualified as Todo
import Example.Page.Todos.TodoCSS qualified as TodoCSS
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.View.Layout as Layout (example, exampleLayout, sourceLink)
import Foreign.Store (Store (..), lookupStore, readStore, storeAction, withStore)
import GHC.Generics (Generic)
import GHC.Word (Word32)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.HTTP.Types (Header, Method, QueryItem, hCacheControl, methodPost, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static as Static (CacheContainer, CachingStrategy (..), Options (..), addBase)
import Network.Wai.Middleware.Static qualified as Static
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Paths_examples (version)
import Safe (readMay)
import System.Environment qualified as SE
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Application
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.OAuth2 (OAuth2, runOAuth2)
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2
import Web.Hyperbole.Server.Options (defaultError)
import Web.Hyperbole.Types.Response

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering

  port <- do
    mStr <- SE.lookupEnv "PORT"
    pure $ fromMaybe 3000 (readMay =<< mStr)
  putStrLn $ "Starting Examples on http://localhost:" <> show port

  users <- Users.initUsers
  (count, chats, config) <- runEff $ runEnvironment $ do
    c <- runConcurrent Effects.initCounter
    ct <- runConcurrent Chat.initChats
    a <- getAppConfigEnv
    pure (c, ct, a)

  cache <- clientCache

  Warp.run port $
    Static.staticPolicyWithOptions cache (addBase "client/dist") $
      Static.staticPolicy (addBase "examples/static") $ do
        exampleApp config users count chats

exampleApp :: AppConfig -> UserStore -> TVar Int -> TVar [(Text, Text)] -> Application
exampleApp config users count chats = do
  liveAppWith
    (ServerOptions (document documentHead) serverError)
    (runApp . routeRequest $ router)
 where
  runApp :: (Hyperbole :> es, IOE :> es) => Eff (OAuth2 : GenRandom : Concurrent : Debug : Users : Todos : Reader AppConfig : es) a -> Eff es a
  runApp = runReader config . runTodosSession . runUsersIO users . runDebugIO . runConcurrent . runRandom . runOAuth2 config.oauth config.manager

  router :: forall es. (Hyperbole :> es, OAuth2 :> es, Todos :> es, Users :> es, Debug :> es, Concurrent :> es, IOE :> es, GenRandom :> es, Reader AppConfig :> es) => AppRoute -> Eff es Response
  router Chat = runReader chats $ runPage $ Chat.page
  router (Hello h) = runPage $ hello h
  router (Contacts (Contact uid)) = Contact.response uid
  router (Contacts ContactsAll) = runPage Contacts.page
  router Concurrency = runPage Concurrency.page
  router (Data r) =
    case r of
      DataLists -> redirect $ routeUri (Data SortableTable)
      SortableTable -> runPage DataTable.page
      Autocomplete -> runPage Autocomplete.page
      Filter -> runPage Filter.page
      LoadMore -> runPage LoadMore.page
  router Errors = runPage Errors.page
  router (Forms _) = runPage Forms.page
  router Requests = runPage Requests.page
  router Route.Response = redirect (routeUri Requests)
  router (State r) =
    case r of
      StateRoot -> redirect $ routeUri (State Actions)
      Actions -> runPage Actions.page
      Effects -> runReader count $ runPage Effects.page
      Sessions -> runPage Sessions.page
      Query -> runPage Query.page
  router Intro = runPage Intro.page
  router (CSS _) = runPage CSS.page
  router Interactivity = runPage Interactivity.page
  router (Examples BigExamples) = redirect $ routeUri (Examples Todos)
  router (Examples Todos) = runPage Todo.page
  router (Examples TodosCSS) = runPage TodoCSS.page
  router Javascript = runPage Javascript.page
  router OAuth2 = runPage OAuth2.page
  router OAuth2Authenticate = OAuth2.handleRedirect
  router Simple = redirect (routeUri Intro)
  router Counter = redirect (routeUri Intro)
  router Test = runPage Test.page
  router Advanced = runPage Advanced.page
  router Main = do
    redirect (routeUri Intro)

  -- Nested Router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es '[]
  hello RedirectNow = do
    redirect (routeUri $ Hello Redirected)
  hello (Greet who) = do
    pure $ exampleLayout (Hello $ Greet who) $ do
      row ~ gap 6 . pad 10 $ do
        el "Hello:"
        el $ text who
  hello Redirected = do
    pure $ exampleLayout Requests $ el ~ pad 10 $ "You were redirected"

  -- Use the embedded version for real applications (see quickStartDocument).
  -- The link to /hyperbole.js here is just to make local development easier
  documentHead :: View DocumentHead ()
  documentHead = do
    title "Hyperbole Examples"
    mobileFriendly
    stylesheet "/cyber.css"
    script "/hyperbole.js"
    script' scriptLiveReload
    stylesheet "/prism.css"
    script "/prism.js" @ att "defer" ""
    style "body { background-color: #d3dceb }, button { font-family: 'Share Tech Mono'}"
    style cssEmbed

  serverError :: ResponseError -> ServerError
  -- serverError NotFound = ServerError "NotFound" $ Cyber.cyberError "Custom Not Found!"
  serverError (ErrCustom s) = s
  serverError err =
    let msg = defaultErrorMessage err
     in ServerError
          { message = msg
          , body = Cyber.cyberError $ Cyber.glitch msg
          }

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
