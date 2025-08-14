module Web.Hyperbole.Effect.Trigger where

import Data.Aeson
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.Types.Event


-- | Trigger an action for an arbitrary hyper biew
trigger :: (HyperView id es, Hyperbole :> es) => id -> Action id -> Eff es ()
trigger vid act = do
  send $ TriggerAction (TargetViewId $ encodeViewId vid) (toAction act)


pushEvent :: (Hyperbole :> es, ToJSON a) => Text -> a -> Eff es ()
pushEvent nm a = do
  send $ TriggerEvent nm (toJSON a)
