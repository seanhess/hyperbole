module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Response (..), ResponseError (..), TargetViewId (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewId (..), hyperUnsafe)
import Web.Hyperbole.View.Types


-- | Respond with the given view, and stop execution
respondEarly :: (Hyperbole :> es, HyperView id es) => id -> View id () -> Eff es a
respondEarly i vw = do
  let vid = TargetViewId (encodedToText $ toViewId i)
  let res = Response vid $ hyperUnsafe i vw
  send $ RespondEarly res


{- | Respond immediately with 404 Not Found

@
#EMBED Example/Docs/App.hs findUser

#EMBED Example/Docs/App.hs userPage
@
-}
notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondEarly NotFound


-- | Respond immediately with a parse error
parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError = send . RespondEarly . Err . ErrParse


-- | Redirect immediately to the 'Url'
redirect :: (Hyperbole :> es) => URI -> Eff es a
redirect = send . RespondEarly . Redirect


-- | Manually set the response to the given view. Normally you would return a 'View' from 'runPage' instead
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response (TargetViewId "") vw
