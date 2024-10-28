{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Web.Hyperbole.Effect where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Constraint, Type)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.String.Conversions
import Data.Text hiding (show)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types hiding (Query)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..), parseQueryParam)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.Hyperbole.Session as Session
import Web.View


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: [Segment]
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: [(BS.ByteString, BS.ByteString)]
  }
  deriving (Show)


{- | Valid responses for a 'Hyperbole' effect. Use 'notFound', etc instead. Reminds you to use 'load' in your 'Page'

> myPage :: (Hyperbole :> es) => Page es Response
> myPage = do
>   -- compiler error: () does not equal Response
>   pure ()
-}
data Response
  = Response TargetViewId (View () ())
  | NotFound
  | Redirect Url
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrParam Text
  | ErrOther Text
  | ErrNotHandled (Event Text Text)
  | ErrAuth
  deriving (Show)


{- | Hyperbole applications are divided into Pages. Each Page must 'load' the whole page , and 'handle' each /type/ of 'HyperView'

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' pageView

pageView = do
  el_ "My Page"
  'hyper' (Message 1) $ messageView "Starting Message"
@
-}

-- newtype Handle (views :: [Type]) (total :: [Type]) es a = Handle (Eff es a)
--   deriving newtype (Functor, Monad, Applicative)

-- newtype Page views es a = Page (Page' views views es (View (Root views) a))
-- type Page views es a = Handle views views es (View (Root views) a)
newtype Page (es :: [Effect]) (views :: [Type]) = Page (Eff es (View (Root views) ()))


data Handler (view :: Type) :: Effect where
  RespondEvents :: Handler view m ()


type instance DispatchOf (Handler view) = Dynamic


type family Handlers (views :: [Type]) (es :: [Effect]) :: Constraint where
  Handlers '[] es = ()
  Handlers (x ': xs) es = (Handler x :> es, Handlers xs es)


load :: (Hyperbole :> es, Handlers total es) => Eff es (View (Root total) ()) -> Page es total
load run = Page $ do
  r <- request
  case lookupEvent r.query of
    -- Are id and action set to something?
    Just e -> send $ RespondEarly $ Err $ ErrNotHandled e
    Nothing -> run


loadToResponse :: Eff es (View (Root total) ()) -> Eff es Response
loadToResponse run = do
  vw <- run
  let vid = TargetViewId (toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res


-- but we actually have to run the handler here...
-- this IS the handler running
handle
  :: forall id total es
   . (HyperView id, Hyperbole :> es)
  => (id -> Action id -> Eff es (View id ()))
  -> Page (Handler id : es) total
  -> Page es total
handle action (Page inner) = Page $ do
  runHandler action $ do
    send $ RespondEvents @id
    inner


runHandler
  :: forall id es a
   . (HyperView id, Hyperbole :> es)
  => (id -> Action id -> Eff es (View id ()))
  -> Eff (Handler id : es) a
  -> Eff es a
runHandler run = interpret $ \_ -> \case
  RespondEvents -> do
    -- Get an event matching our type. If it doesn't match, skip to the next handler
    mev <- getEvent @id :: Eff es (Maybe (Event id (Action id)))
    case mev of
      Just event -> do
        vw <- run event.viewId event.action
        let vid = TargetViewId $ toViewId event.viewId
        send $ RespondEarly $ Response vid $ hyperUnsafe event.viewId vw
      _ -> do
        pure ()


-- deriving newtype (Applicative, Monad, Functor)

-- | Serialized ViewId
newtype TargetViewId = TargetViewId Text


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Session -> Response -> Server m ()


type instance DispatchOf Server = 'Dynamic


{- | In any 'load' or 'handle', you can use this Effect to get extra request information or control the response manually.

For most 'Page's, you won't need to use this effect directly. Use custom 'Route's for request info, and return 'View's to respond
-}
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondEarly :: Response -> Hyperbole m a
  SetSession :: (ToHttpApiData a) => Text -> a -> Hyperbole m ()
  DelSession :: Text -> Hyperbole m ()
  GetSession :: (FromHttpApiData a) => Text -> Hyperbole m (Maybe a)


type instance DispatchOf Hyperbole = 'Dynamic


data HyperState = HyperState
  { request :: Request
  , session :: Session
  }


-- | Run the 'Hyperbole' effect to 'Server'
runHyperbole
  :: (Server :> es)
  => Eff (Hyperbole : es) Response
  -> Eff es Response
runHyperbole = fmap combine $ reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    gets @HyperState (.request)
  RespondEarly r -> do
    s <- gets @HyperState (.session)
    send $ SendResponse s r
    throwError_ r
  SetSession k a -> do
    modify $ \st -> st{session = sessionSet k a st.session} :: HyperState
  DelSession k -> do
    modify $ \st -> st{session = sessionDel k st.session} :: HyperState
  GetSession k -> do
    s <- gets @HyperState (.session)
    pure $ sessionLookup k s
 where
  runLocal :: (Server :> es) => Eff (State HyperState : Error Response : es) a -> Eff es (Either Response (a, HyperState))
  runLocal eff = do
    -- Load the request ONCE right when we start
    r <- send LoadRequest
    let st = HyperState r (sessionFromCookies r.cookies)
    runErrorNoCallStack @Response . runState st $ eff

  combine :: (Server :> es) => Eff es (Either Response (Response, HyperState)) -> Eff es Response
  combine eff = do
    er <- eff
    case er of
      Left res ->
        -- responded early, don't need to respond again
        pure res
      Right (res, st) -> do
        send $ SendResponse st.session res
        pure res


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es [Segment]
reqPath = (.path) <$> request


{- | Return the request body as a Web.FormUrlEncoded.Form

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Hyperbole :> es) => Eff es Form
formBody = do
  b <- (.body) <$> request
  let ef = urlDecodeForm b
  -- not going to work. we need a way to `throwError` or it doesn't work...
  either (send . RespondEarly . Err . ErrParse) pure ef


getEvent :: (HyperView id, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- reqParams
  pure $ parseEvent q


parseEvent :: (HyperView id) => Query -> Maybe (Event id (Action id))
parseEvent q = do
  Event ti ta <- lookupEvent q
  vid <- parseViewId ti
  act <- parseAction ta
  pure $ Event vid act


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q' =
  Event
    <$> lookupParam "id" q'
    <*> lookupParam "action" q'


{- | Lookup a session variable by keyword

> load $ do
>   tok <- session "token"
>   ...
-}
session :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es (Maybe a)
session k = send $ GetSession k


{- | Set a session variable by keyword

> load $ do
>   t <- reqParam "token"
>   setSession "token" t
>   ...
-}
setSession :: (Hyperbole :> es, ToHttpApiData a) => Text -> a -> Eff es ()
setSession k v = send $ SetSession k v


-- | Clear the user's session
clearSession :: (Hyperbole :> es) => Text -> Eff es ()
clearSession k = send $ DelSession k


{- | Return the entire 'Query'

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    q <- reqParams
    case 'lookupParam' "token" q of
      Nothing -> pure $ errorView "Missing Token in Query String"
      Just t -> do
        sideEffectUsingToken token
        pure myPageView
@
-}
reqParams :: (Hyperbole :> es) => Eff es Query
reqParams = (.query) <$> request


{- | Require a given parameter from the 'Query' arguments

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    token <- reqParam "token"
    sideEffectUsingToken token
    pure myPageView
@
-}
reqParam :: forall a es. (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
reqParam p = do
  q <- reqParams
  (er :: Either Response a) <- pure $ do
    mv <- require $ List.lookup (encodeUtf8 p) q
    v <- require mv
    first (Err . ErrParam) $ parseQueryParam (decodeUtf8 v)
  case er of
    Left e -> send $ RespondEarly e
    Right a -> pure a
 where
  require :: Maybe x -> Either Response x
  require Nothing = Left $ Err $ ErrParam $ "Missing: " <> p
  require (Just a) = pure a


-- | Lookup the query param in the 'Query'
lookupParam :: BS.ByteString -> Query -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q


-- | Whether the param is present or not
hasParam :: BS.ByteString -> Query -> Bool
hasParam p q =
  isJust $ lookup p q


{- | Respond immediately with 404 Not Found

@
userLoad :: (Hyperbole :> es, Users :> es) => UserId -> Eff es User
userLoad uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu

myPage :: (Hyperbole :> es, Users :> es) => Eff es View
myPage = do
  load $ do
    u <- userLoad 100
    -- skipped if user = Nothing
    pure $ userView u
@
-}
notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondEarly NotFound


-- | Respond immediately with a parse error
parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError = send . RespondEarly . Err . ErrParse


-- | Redirect immediately to the 'Url'
redirect :: (Hyperbole :> es) => Url -> Eff es a
redirect = send . RespondEarly . Redirect


-- | Respond with the given view, and stop execution
respondEarly :: (Hyperbole :> es, HyperView id) => id -> View id () -> Eff es ()
respondEarly i vw = do
  let vid = TargetViewId (toViewId i)
  let res = Response vid $ addContext i vw
  send $ RespondEarly res


-- | Manually set the response to the given view. Normally you return a 'View' from 'load' or 'handle' instead of using this
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response (TargetViewId "") vw


{- | The load handler is run when the page is first loaded. Run any side effects needed, then return a view of the full page

@
myPage :: (Hyperbole :> es) => UserId -> Page es Response
myPage userId = do
  'load' $ do
    user <- loadUserFromDatabase userId
    pure $ userPageView user
@
-}

-- load
--   :: (Hyperbole :> es)
--   => Eff es (View (Root views) ())
--   -> Page views es Response
-- load run = Page $ do
--   r <- request
--   case lookupEvent r.query of
--     -- Are id and action set to sometjhing?
--     Just e ->
--       pure $ Err $ ErrNotHandled e
--     Nothing -> do
--       vw <- run
--       view vw

{- | A handler is run when an action for that 'HyperView' is triggered. Run any side effects needed, then return a view of the corresponding type

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' pageView

messages :: ('Hyperbole' :> es, MessageDatabase) => Message -> MessageAction -> 'Eff' es ('View' Message ())
messages (Message mid) ClearMessage = do
  deleteMessageSideEffect mid
  pure $ messageView ""

messages (Message mid) (Louder m) = do
  let new = m <> "!"
  saveMessageSideEffect mid new
  pure $ messageView new
@
-}

-- runHandler
--   :: forall id es
--    . (Hyperbole :> es, HyperView id)
--   => (id -> Action id -> Eff es (View id ()))
--   -> Eff es ()
-- runHandler run = do
--   -- Get an event matching our type. If it doesn't match, skip to the next handler
--   mev <- getEvent @id
--   case mev of
--     Just event -> do
--       vw <- run event.viewId event.action
--       let vid = TargetViewId $ toViewId event.viewId
--       send $ RespondEarly $ Response vid $ hyperUnsafe event.viewId vw
--     _ -> pure ()

-- | Run a 'Page' in 'Hyperbole'
page
  :: forall views es
   . (Hyperbole :> es)
  => Page es views
  -> Eff es Response
page (Page eff) = do
  loadToResponse eff
