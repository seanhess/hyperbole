module Web.Hyperbole.Effect.Event where

import Data.Text (Text)
import Effectful
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.Effect.Request (lookupParam, reqParams)
import Web.Hyperbole.Effect.Server (Event (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..), ViewId (..))
import Web.View (Query)


getEvent :: (HyperView id es, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- reqParams
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- parseViewId ti
    act <- parseAction ta
    pure $ Event vid act


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q' =
  Event
    <$> lookupParam "id" q'
    <*> lookupParam "action" q'
