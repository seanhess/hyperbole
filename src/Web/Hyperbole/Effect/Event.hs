module Web.Hyperbole.Effect.Event (getEvent) where

import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Event (..), Request (..), lookupEvent)
import Web.Hyperbole.HyperView (HyperView (..))
import Web.Hyperbole.HyperView.Types (decodeAction, decodeViewId)


getEvent :: (HyperView id es, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- (.query) <$> send GetRequest
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- decodeViewId ti
    act <- decodeAction ta
    pure $ Event vid act
