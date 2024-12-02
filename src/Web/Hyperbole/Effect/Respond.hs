module Web.Hyperbole.Effect.Respond where

import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Response (..), TargetViewId (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewId (..), hyperUnsafe)
import Web.View (View)


-- | Respond with the given view, and stop execution
respondEarly :: (Hyperbole :> es, HyperView id es) => id -> View id () -> Eff es ()
respondEarly i vw = do
  let vid = TargetViewId (toViewId i)
  let res = Response vid $ hyperUnsafe i vw
  send $ RespondEarly res
