{-# LANGUAGE LambdaCase #-}

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
import Effectful.Reader.Static
import Network.HTTP.Types (Method, Query)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.View


data Request = Request
  { path :: [Text]
  , query :: Query
  , body :: BL.ByteString
  , method :: Maybe Method
  }
  deriving (Show)


data Response
  = ErrParse Text
  | ErrParam Text
  | Response (View () ())
  | NotFound


newtype Page es a = Page (Eff es a)
  deriving newtype (Applicative, Monad, Functor)


data Event act id = Event
  { viewId :: id
  , action :: act
  }


data Hyperbole :: Effect where
  QueryParams :: Hyperbole m Query
  QueryParam :: (FromHttpApiData a) => Text -> Hyperbole m a
  FormData :: Hyperbole m Form
  ReqPath :: Hyperbole m [Text]
  GetEvent :: (HyperView id) => Hyperbole m (Maybe (Event (Action id) id))
  RespondEarly :: Response -> Hyperbole m a


-- ParseError :: HyperError -> Hyperbole m a

type instance DispatchOf Hyperbole = 'Dynamic


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
  path <- send ReqPath
  case findRoute path of
    Nothing -> pure NotFound
    Just rt -> actions rt


-- er <- runHyperbole req (actions rt)
-- either pure pure er

runHyperbole
  :: Request
  -> Eff (Hyperbole : es) a
  -> Eff es (Either Response a)
runHyperbole req =
  reinterpret runLocal $ \_ -> \case
    QueryParams -> getQuery
    QueryParam p -> getParam p
    FormData -> getForm
    ReqPath -> pure req.path
    GetEvent -> getEvent
    RespondEarly r -> respond r
 where
  respond :: (Error Response :> es) => Response -> Eff es a
  respond = throwError

  runLocal =
    runErrorNoCallStack @Response
      . runReader req

  getQuery :: (Reader Request :> es, Error Response :> es) => Eff es Query
  getQuery = do
    asks @Request (.query)

  getParam :: forall es a. (Reader Request :> es, Error Response :> es, FromHttpApiData a) => Text -> Eff es a
  getParam p = do
    (q :: Query) <- asks @Request (.query)
    either throwError pure $ do
      mv <- require $ List.lookup (encodeUtf8 p) q
      v <- require mv
      first ErrParam $ parseQueryParam (decodeUtf8 v)
   where
    require :: Maybe x -> Either Response x
    require Nothing = Left $ ErrParam $ "Missing: " <> p
    require (Just a) = pure a

  getForm :: (Reader Request :> es, Error Response :> es) => Eff es Form
  getForm = do
    b <- asks @Request (.body)
    let ef = urlDecodeForm b
    either (respond . ErrParse) pure ef

  getEvent :: (Reader Request :> es, HyperView id) => Eff es (Maybe (Event (Action id) id))
  getEvent = do
    q <- asks @Request (.query)
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


queryParams :: (Hyperbole :> es) => Eff es Query
queryParams = send QueryParams


queryParam :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
queryParam = send . QueryParam


formData :: (Hyperbole :> es) => Eff es Form
formData = send FormData


notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondEarly NotFound


parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError e = send $ RespondEarly $ ErrParse e


-- | Set the response to the view. Note that `page` already expects a view to be returned from the effect
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = pure $ Response vw


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
  mev <- send GetEvent
  case mev of
    Just event -> do
      vw <- run event.viewId event.action
      send $ RespondEarly $ Response $ viewId event.viewId vw
    _ -> pure ()


page
  :: (Hyperbole :> es)
  => Page es Response
  -> Eff es Response
page (Page eff) = eff
