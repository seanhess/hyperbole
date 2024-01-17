{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect where

import Control.Monad (join)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Network.HTTP.Types (Query)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.View


data Request = Request
  { path :: [Text]
  , query :: Query
  , body :: BL.ByteString
  }
  deriving (Show)


data Response
  = ErrParse Text
  | ErrNoHandler
  | Response (View () ())
  | NotFound


newtype Page es a = Page (Eff es a)
  deriving newtype (Applicative, Monad, Functor)


data Event act id = Event
  { viewId :: id
  , action :: act
  }


data Hyperbole :: Effect where
  GetForm :: Hyperbole m Form
  GetEvent :: (HyperView id) => Hyperbole m (Maybe (Event (Action id) id))
  Respond :: Response -> Hyperbole m a


-- ParseError :: HyperError -> Hyperbole m a

type instance DispatchOf Hyperbole = 'Dynamic


runHyperboleRoute
  :: (Route route)
  => Request
  -> (route -> Eff (Hyperbole : es) ())
  -> Eff es Response
runHyperboleRoute req actions = do
  case findRoute req.path of
    Nothing -> pure NotFound
    Just rt -> do
      er <- runHyperbole req (actions rt)
      case er of
        Left r -> pure r
        Right _ -> pure ErrNoHandler


runHyperbole
  :: Request
  -> Eff (Hyperbole : es) a
  -> Eff es (Either Response a)
runHyperbole req =
  reinterpret runLocal $ \_ -> \case
    GetForm -> getForm
    GetEvent -> getEvent
    Respond r -> respond r
 where
  respond :: (Error Response :> es) => Response -> Eff es a
  respond = throwError

  runLocal =
    runErrorNoCallStack @Response
      . runReader req

  getForm :: (Reader Request :> es, Error Response :> es) => Eff es Form
  getForm = do
    bd <- asks @Request (.body)
    let ef = urlDecodeForm bd
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


formData :: (Hyperbole :> es) => Eff es Form
formData = send GetForm


notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ Respond NotFound


parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError e = send $ Respond $ ErrParse e


-- | Set the response to the view. Note that `page` already expects a view to be returned from the effect
view :: (Hyperbole :> es) => View () () -> Eff es ()
view vw = send $ Respond $ Response vw


-- | Load the entire page when no HyperViews match
load
  :: (Hyperbole :> es)
  => Eff es (View () ())
  -> Page es ()
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
      view $ viewId event.viewId vw
    _ -> pure ()


page
  :: (Hyperbole :> es)
  => Page es ()
  -> Eff es ()
page (Page eff) = eff
