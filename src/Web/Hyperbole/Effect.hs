{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoFieldSelectors #-}

module Web.Hyperbole.Effect where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import Data.String.Conversions
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types hiding (Query)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..), parseQueryParam)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Param (Param (..))
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
  = Response (View () ())
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
newtype Page es a = Page (Eff es a)
  deriving newtype (Applicative, Monad, Functor)


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
    throwError r
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
formData :: (Hyperbole :> es) => Eff es Form
formData = do
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


-- | Lookup the query param in the 'Query'
lookupParam :: BS.ByteString -> Query -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q


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
reqParam :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
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
respondEarly vid vw = do
  let res = Response $ hyper vid vw
  send $ RespondEarly res


-- | Manually set the response to the given view. Normally you return a 'View' from 'load' or 'handle' instead of using this
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response vw


{- | The load handler is run when the page is first loaded. Run any side effects needed, then return a view of the full page

@
myPage :: (Hyperbole :> es) => UserId -> Page es Response
myPage userId = do
  'load' $ do
    user <- loadUserFromDatabase userId
    pure $ userPageView user
@
-}
load
  :: (Hyperbole :> es)
  => Eff es (View () ())
  -> Page es Response
load run = Page $ do
  r <- request
  case lookupEvent r.query of
    -- Are id and action set to sometjhing?
    Just e ->
      pure $ Err $ ErrNotHandled e
    Nothing -> do
      vw <- run
      view vw


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
handle
  :: forall id es
   . (Hyperbole :> es, HyperView id)
  => (id -> Action id -> Eff es (View id ()))
  -> Page es ()
handle run = Page $ do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent @id
  case mev of
    Just event -> do
      vw <- run event.viewId event.action
      send $ RespondEarly $ Response $ hyper event.viewId vw
    _ -> pure ()


-- | Run a 'Page' in 'Hyperbole'
page
  :: (Hyperbole :> es)
  => Page es Response
  -> Eff es Response
page (Page eff) = eff
