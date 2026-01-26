{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module App where

import App.Cache (clientCache)
import App.Config
import App.Docs.Page
import App.Page.Application qualified as Application
import App.Page.CSS qualified as CSS
import App.Page.Concurrency qualified as Concurrency
import App.Page.Examples qualified as Examples
import App.Page.Forms qualified as Forms
import App.Page.HyperboleEffect qualified as Hyp
import App.Page.Hyperviews qualified as Hyperviews
import App.Page.Interactivity qualified as Interactivity
import App.Page.Intro.Basics qualified as Basics
import App.Page.Intro.Intro qualified as Intro
import App.Page.OAuth2 qualified as OAuth2
import App.Page.SideEffects qualified as SideEffects
import App.Page.State qualified as State
import App.Page.ViewFunctions qualified as ViewFunctions
import App.Route as Route
import Control.Concurrent
  ( MVar
  , ThreadId
  , forkFinally
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Monad (forever, when, (>=>))
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
import Example.Chat qualified as Chat
import Example.Colors
import Example.Contact qualified as Contact
import Example.Contacts qualified as Contacts
import Example.Counter qualified as Counter
import Example.DataLists.Autocomplete qualified as Autocomplete
import Example.DataLists.DataTable qualified as DataTable
import Example.DataLists.Filter qualified as Filter
import Example.DataLists.LoadMore qualified as LoadMore
import Example.Effects.Debug as Debug
import Example.Effects.Todos (Todos, runTodosSession)
import Example.Effects.Users as Users
import Example.Scrollbars qualified as Scrollbars
import Example.State.Effects qualified as Effects
import Example.State.Query qualified as Query
import Example.State.Sessions qualified as Sessions
import Example.Style qualified as Style
import Example.Style.Cyber qualified as Cyber
import Example.Tags qualified as Tags
import Example.Test qualified as Test
import Example.Todos.Todo qualified as Todo
import Example.Todos.TodoCSS qualified as TodoCSS
import Example.View.Layout as Layout (layout)
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
import Paths_demo (version)
import Paths_demo qualified as Pt
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
  (count, room, config) <- runEff $ runEnvironment $ do
    c <- runConcurrent Effects.initCounter
    room <- runConcurrent Chat.initChatRoom
    a <- getAppConfigEnv
    pure (c, room, a)

  cache <- clientCache

  Warp.run port $
    Static.staticPolicyWithOptions cache (addBase "client/dist") $
      Static.staticPolicy (addBase "demo/static") $ do
        devReload config $ exampleApp config users count room
 where
  devReload :: AppConfig -> Application -> Application
  devReload config
    | config.devMode = Wai.modifyResponse $ Wai.mapResponseHeaders $ \hs -> ("Connection", "Close") : hs
    | otherwise = id

exampleApp :: AppConfig -> UserStore -> TVar Int -> Chat.Room -> Application
exampleApp config users count chats = do
  liveAppWith
    (ServerOptions (document documentHead) serverError)
    (runApp . routeRequest $ router)
 where
  runApp :: (Hyperbole :> es, IOE :> es) => Eff (OAuth2 : GenRandom : Concurrent : Debug : Users : Todos : Reader AppConfig : es) a -> Eff es a
  runApp = runReader config . runTodosSession . runUsersIO users . runDebugIO . runConcurrent . runRandom . runOAuth2 config.oauth config.manager

  router :: forall es. (Hyperbole :> es, OAuth2 :> es, Todos :> es, Users :> es, Debug :> es, Concurrent :> es, IOE :> es, GenRandom :> es, Reader AppConfig :> es) => AppRoute -> Eff es Response
  router Counter = runPage Counter.page
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
  router Errors = redirect (routeUri HyperboleEffect)
  router (Forms _) = runPage Forms.page
  router HyperboleEffect = runPage Hyp.page
  router Hyperviews = runPage Hyperviews.page
  router Route.Response = redirect (routeUri HyperboleEffect)
  router State = runReader count $ runPage State.page
  router SideEffects = runReader @Text "Secret Message!" $ runPage SideEffects.page
  router Intro = runPage Intro.page
  router Basics = runPage Basics.page
  router Application = runPage Application.page
  router ViewFunctions = runPage ViewFunctions.page
  -- router (Intro HyperViews) = runPage IntroHyperViews.page
  -- router (Intro Pages) = runPage IntroPages.page
  -- router (Intro ViewFunctions) = runPage IntroViewFunctions.page
  router CSS = runPage CSS.page
  router Interactivity = runPage Interactivity.page
  router (Examples Chat) = runReader chats $ runPage Chat.page
  router (Examples OtherExamples) = runPage Examples.page
  router (Examples Todos) = runPage Todo.page
  router (Examples Tags) = runPage Tags.page
  router (Examples TodosCSS) = runPage TodoCSS.page
  router Javascript = redirect (routeUri Interactivity)
  router (Examples OAuth2) = runPage OAuth2.page
  router (Examples OAuth2Authenticate) = OAuth2.handleRedirect
  router (Examples Scrollbars) = runPage Scrollbars.page
  router Simple = redirect (routeUri Intro)
  -- router Counter = redirect (routeUri $ State StateRoot)
  router Test = runPage Test.page
  router Main = do
    redirect (routeUri Intro)

  -- Nested Router
  hello :: (Hyperbole :> es, Debug :> es) => Hello -> Page es '[]
  hello RedirectNow = do
    redirect (routeUri $ Hello Redirected)
  hello (Greet who) = do
    pure $ layout (Hello $ Greet who) $ do
      row ~ gap 6 . pad 10 $ do
        el "Hello:"
        el $ text who
  hello Redirected = do
    pure $ layout HyperboleEffect $ do
      col ~ pad 10 . gap 10 $ do
        el "You were redirected"
        route HyperboleEffect ~ Style.link $ "Go Back"

  -- Use the embedded version for real applications (see quickStartDocument).
  -- The link to /hyperbole.js here is just to make local development easier
  documentHead :: View DocumentHead ()
  documentHead = do
    title "Hyperbole Examples"
    mobileFriendly
    stylesheet "/cyber.css"
    script "/hyperbole.js"
    stylesheet "/prism.css"
    script "/prism.js" @ att "defer" ""
    script "/docs.js" @ att "defer" ""
    style "html { scroll-behavior: smooth; }\n body { background-color: #e0e7f1; font-family: font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Noto Sans\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\") }, button { font-family: 'Share Tech Mono'}"
    style cssEmbed

    when config.devMode $ do
      script' scriptLiveReload

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
      App.run
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
