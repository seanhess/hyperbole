module Web.Hyperbole.Effect.Event where

import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (Query)
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Server (Event (..), Request (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..), ViewId (..))


getEvent :: (HyperView id es, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- (.query) <$> request
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- parseViewId ti
    act <- parseAction ta
    pure $ Event vid act


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q = do
  viewId <- lookupParam "hyp-id" q
  action <- lookupParam "hyp-action" q
  pure $ Event{viewId, action}


-- | Lower-level lookup straight from the request
lookupParam :: ByteString -> Query -> Maybe Text
lookupParam key q = do
  mval <- lookup key q
  val <- mval
  pure $ cs val
