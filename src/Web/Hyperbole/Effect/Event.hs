module Web.Hyperbole.Effect.Event where

import Control.Monad (join)
import Data.String.Conversions
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (QueryText)
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.Effect.Request (reqParams)
import Web.Hyperbole.Effect.Server (Event (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..), ViewId (..))


getEvent :: (HyperView id es, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- reqParams
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- parseViewId ti
    act <- parseAction ta
    pure $ Event vid act


lookupEvent :: QueryText -> Maybe (Event Text Text)
lookupEvent q' =
  Event
    <$> lookupParam "id" q'
    <*> lookupParam "action" q'

-- -- | Lookup the query param in the 'Query'
lookupParam :: Text -> QueryText -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q