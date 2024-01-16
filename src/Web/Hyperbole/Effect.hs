{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Effect where

import Control.Monad (join)
import Data.ByteString
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Wai (ContentType (..), Wai (..))
import Effectful.Wai qualified as Wai
import Network.HTTP.Types (Query)
import Network.Wai
import Web.FormUrlEncoded (Form)
import Web.FormUrlEncoded qualified as Form
import Web.Hyperbole.HyperView
import Web.Hyperbole.Socket
import Web.View


data Hyperbole :: Effect where
  GetForm :: Hyperbole m Form
  ParseForm :: (Form.FromForm a) => Hyperbole m a
  GetEvent :: (HyperView id) => Hyperbole m (Maybe (Event (Action id) id))
  RespondView :: View () () -> Hyperbole m ()
  HyperError :: HyperError -> Hyperbole m a


type instance DispatchOf Hyperbole = 'Dynamic


data Event act id = Event
  { viewId :: id
  , action :: act
  }


runHyperboleSocket
  :: (Socket :> es)
  => Eff (Hyperbole : es) a
  -> Eff es a
runHyperboleSocket = _


runHyperboleWai
  :: (Wai :> es)
  => Eff (Hyperbole : es) a
  -> Eff es a
runHyperboleWai = interpret $ \_ -> \case
  RespondView vw -> do
    let bd = renderLazyByteString vw
    send $ ResHeader "Content-Type" "text/html"
    send $ ResBody ContentHtml bd
    Wai.continue
  GetForm -> Wai.formData
  ParseForm -> Wai.parseFormData
  HyperError NotFound -> send $ Interrupt Wai.NotFound
  HyperError (ParseError e) -> send $ Interrupt $ Wai.ParseError e
  GetEvent -> do
    q <- fmap queryString <$> send $ Wai.Request
    pure $ do
      Event ti ta <- lookupEvent q
      vid <- parseParam ti
      act <- parseParam ta
      pure $ Event vid act
 where
  lookupParam :: ByteString -> Query -> Maybe Text
  lookupParam p q =
    fmap cs <$> join $ lookup p q

  lookupEvent :: Query -> Maybe (Event Text Text)
  lookupEvent q =
    Event
      <$> lookupParam "id" q
      <*> lookupParam "action" q


formData :: (Hyperbole :> es) => Eff es Form
formData = send GetForm


parseFormData :: (Hyperbole :> es, Form.FromForm a) => Eff es a
parseFormData = send ParseForm


-- | Read a required form parameter
param :: (Hyperbole :> es, Param a) => Text -> Form -> Eff es a
param p f = do
  -- param is required
  either (send . HyperError . ParseError) pure $ do
    t <- Form.lookupUnique p f
    maybe (Left [i|could not parseParam: '#{t}'|]) pure $ parseParam t


notFound :: (Hyperbole :> es) => Eff es a
notFound = send (HyperError NotFound)


-- | Set the response to the view. Note that `page` already expects a view to be returned from the effect
view :: (Hyperbole :> es) => View () () -> Eff es ()
view = send . RespondView


data HyperError
  = NotFound
  | ParseError Text


newtype Page es a = Page (Eff es a)
  deriving newtype (Applicative, Monad, Functor)


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
