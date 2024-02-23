{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
import Network.HTTP.Types
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.View


data Request = Request
  { path :: [Text]
  , query :: Query
  , body :: BL.ByteString
  , isFullPage :: Bool
  }
  deriving (Show)


data Response
  = Response (View () ())
  | NotFound
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrParam Text
  | ErrOther Text
  deriving (Show)


newtype Page es a = Page (Eff es a)
  deriving newtype (Applicative, Monad, Functor)


data Event act id = Event
  { viewId :: id
  , action :: act
  }


-- runHyperboleRoute
--   :: (Route route)
--   => Request
--   -> (route -> Eff (Hyperbole : es) Response)
--   -> Eff es Response
-- runHyperboleRoute req actions = do
--   case findRoute req.path of
--     Nothing -> pure NotFound
--     Just rt -> do
--       er <- runHyperbole req (actions rt)
--       either pure pure er

routeRequest :: (Hyperbole :> es, Route route) => (route -> Eff es Response) -> Eff es Response
routeRequest actions = do
  path <- reqPath
  case findRoute path of
    Nothing -> send $ Respond NotFound
    Just rt -> actions rt


-- er <- runHyperbole req (actions rt)
-- either pure pure er

-- runHyperboleResponse
--   :: Request
--   -> Eff (Hyperbole : es) Response
--   -> Eff es Response
-- runHyperboleResponse req actions = do
--   res <- runHyperbole req actions
--   either pure pure res
--
newtype Session = Session [(Text, Text)]


setSession :: Text -> Text -> Session -> Session
setSession k v (Session kvs) = Session $ (k, v) : kvs


-- whoever USES this guarantees they are only called once
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Session -> Response -> Server m ()
type instance DispatchOf Server = 'Dynamic


data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  Respond :: Response -> Hyperbole m a
  SetSession :: Text -> Text -> Hyperbole m ()
type instance DispatchOf Hyperbole = 'Dynamic


data HyperState = HyperState
  { request :: Maybe Request
  , session :: Session
  }


runHyperbole
  :: (Server :> es)
  => Eff (Hyperbole : es) Response
  -> Eff es Response
runHyperbole = fmap combine $ reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    mr <- gets @HyperState (.request)
    case mr of
      Just r -> pure r
      Nothing -> do
        r <- send LoadRequest
        modify $ \s -> s{request = Just r}
        pure r
  Respond r -> do
    -- TODO: build response here!
    s <- gets @HyperState (.session)
    send $ SendResponse s r
    throwError r
  SetSession k v -> do
    modify $ \s -> s{session = setSession k v s.session}
 where
  runLocal :: Eff (State HyperState : Error Response : es) a -> Eff es (Either Response a)
  runLocal = runErrorNoCallStack @Response . evalState (HyperState Nothing (Session []))

  combine :: Eff es (Either Response Response) -> Eff es Response
  combine eff = do
    err <- eff
    either pure pure err


-- e <- eff
-- either id id e

-- returnEarly :: (Hyperbole :> es) => Response -> Eff es ()
-- returnEarly = _

request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


reqPath :: (Hyperbole :> es) => Eff es [Text]
reqPath = (.path) <$> request


formData :: (Hyperbole :> es) => Eff es Form
formData = do
  b <- (.body) <$> request
  let ef = urlDecodeForm b
  -- not going to work. we need a way to `throwError` or it doesn't work...
  either (send . Respond . Err . ErrParse) pure ef


getEvent :: (Hyperbole :> es, HyperView id) => Eff es (Maybe (Event (Action id) id))
getEvent = do
  q <- reqParams
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- parseParam ti
    act <- parseParam ta
    pure $ Event vid act


lookupParam :: BS.ByteString -> Query -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q =
  Event
    <$> lookupParam "id" q
    <*> lookupParam "action" q


reqParams :: (Hyperbole :> es) => Eff es Query
reqParams = (.query) <$> request


reqParam :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
reqParam p = do
  q <- reqParams
  (er :: Either Response a) <- pure $ do
    mv <- require $ List.lookup (encodeUtf8 p) q
    v <- require mv
    first (Err . ErrParam) $ parseQueryParam (decodeUtf8 v)
  case er of
    Left e -> send $ Respond e
    Right a -> pure a
 where
  require :: Maybe x -> Either Response x
  require Nothing = Left $ Err $ ErrParam $ "Missing: " <> p
  require (Just a) = pure a


notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ Respond NotFound


parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError = send . Respond . Err . ErrParse


-- | Set the response to the view. Note that `page` already expects a view to be returned from the effect
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  send $ Respond $ Response vw


-- | Load the entire page when no HyperViews match
load
  :: (Hyperbole :> es)
  => Eff es (View () ())
  -> Page es Response
load run = Page $ do
  vw <- run
  view vw


-- | Handle a HyperView. If the event matches our handler, respond with the fragment
hyper
  :: (Hyperbole :> es, HyperView id)
  => (id -> Action id -> Eff es (View id ()))
  -> Page es ()
hyper run = Page $ do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent
  case mev of
    Just event -> do
      vw <- run event.viewId event.action
      send $ Respond $ Response $ viewId event.viewId vw
    _ -> pure ()


page
  :: (Hyperbole :> es)
  => Page es Response
  -> Eff es Response
page (Page eff) = eff


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")
