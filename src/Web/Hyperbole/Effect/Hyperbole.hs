{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Hyperbole where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..), parseQueryParam)
import Web.Hyperbole.Component (Component (..))
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Handler.TypeList (HyperViewHandled)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.Hyperbole.Session as Session
import Web.Hyperbole.View.Target (hyperUnsafe)
import Web.View


hyper
  :: forall c ctx es
   . ( HyperViewHandled c ctx es
     , ViewId c
     , Component c es
     )
  => c
  -> View c ()
  -> View ctx ()
hyper = hyperUnsafe @c @es


-- | Initialize a given component giving it an initial value.
start
  :: forall c ctx es
   . ( HyperViewHandled c ctx es
     , ViewId c
     , Component c es
     )
  => c
  -> Model c
  -> View ctx ()
start componentId model =
  hyperUnsafe @c @es componentId $ render @c @es model


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


data HyperState = HyperState
  { request :: Request
  , session :: Session
  }


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


getEvent :: forall id es. (Read (Action id), Component id es, Hyperbole :> es, ViewId id) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- reqParams
  pure $ parseEvent @id @es q


parseEvent :: (Read (Action id), Component id es, ViewId id) => Query -> Maybe (Event id (Action id))
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
respondEarly :: forall id es. (Component id es, Hyperbole :> es, ViewId id) => id -> View id () -> Eff es ()
respondEarly i vw = do
  let vid = TargetViewId (toViewId i)
  let res = Response vid $ hyperUnsafe @id @es i vw
  send $ RespondEarly res


-- | Manually set the response to the given view. Normally you return a 'View' from 'load' or 'handle' instead of using this
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response (TargetViewId "") vw
