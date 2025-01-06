module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Response (..), ResponseError (..), TargetViewId (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewId (..), hyperUnsafe)
import Web.View (Url, View)


-- | Respond with the given view, and stop execution
respondEarly :: (Hyperbole :> es, HyperView id es) => id -> View id () -> Eff es ()
respondEarly i vw = do
  let vid = TargetViewId (toViewId i)
  let res = Response vid $ hyperUnsafe i vw
  send $ RespondEarly res


{- | Respond immediately with 404 Not Found

@
userLoad :: (Hyperbole :> es, Users :> es) => UserId -> Eff es User
userLoad uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu

myPage :: (Hyperbole :> es, Users :> es) => Eff es View
myPage = do
  load $ do
    u <- userLoad 100
    -- skipped if user = Nothing
    pure $ userView u
@
-}
notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondEarly NotFound


-- | Respond immediately with a parse error
parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError = send . RespondEarly . Err . ErrParse


-- | Redirect immediately to the 'Url'
redirect :: (Hyperbole :> es) => Url -> Eff es a
redirect = send . RespondEarly . Redirect


-- | Manually set the response to the given view. Normally you return a 'View' from 'load' or 'handle' instead of using this
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response (TargetViewId "") vw
