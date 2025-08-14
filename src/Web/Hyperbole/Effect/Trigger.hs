module Web.Hyperbole.Effect.Trigger where

import Data.Aeson
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request


trigger :: (HyperView id es, Hyperbole :> es) => id -> Action id -> Eff es ()
trigger vid act = send $ TriggerAction (TargetViewId $ encodeViewId vid) (toAction act)


pushEvent :: (Hyperbole :> es, ToJSON a) => Text -> a -> Eff es ()
pushEvent nm a = do
  vid <- currentViewId
  send $ TriggerEvent vid nm (toJSON a)
 where
  currentViewId = do
    mev <- (.event) <$> send GetRequest
    pure $ do
      ev <- mev
      pure ev.viewId
