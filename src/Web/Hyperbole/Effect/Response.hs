module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.HyperView (ConcurrencyValue (..), HyperView (..), ViewId (..), hyperUnsafe)
import Web.Hyperbole.HyperView.ViewId (viewId)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View.Types


-- | Respond with the given hyperview
hyperView :: (HyperView id es) => id -> View id () -> Eff es Response
hyperView i vw = do
  let vid = TargetViewId (toViewId i)
  pure $ Response vid $ hyperUnsafe i vw


pushUpdate :: (Hyperbole :> es, ViewId id, ConcurrencyValue (Concurrency id)) => View id () -> Eff (Reader id : es) ()
pushUpdate vw = do
  i <- viewId
  pushUpdateTo i vw


pushUpdateTo :: (Hyperbole :> es, ViewId id, ConcurrencyValue (Concurrency id)) => id -> View id () -> Eff es ()
pushUpdateTo i vw = do
  send $ PushUpdate (TargetViewId $ toViewId i) $ hyperUnsafe i vw


-- | Abort execution and respond with an error
respondError :: (Hyperbole :> es) => ResponseError -> Eff es a
respondError err = do
  send $ RespondNow $ Err err


-- | Abort execution and respond with an error view
respondErrorView :: (Hyperbole :> es) => Text -> View Body () -> Eff es a
respondErrorView msg vw = do
  send $ RespondNow $ Err $ ErrCustom $ ServerError msg vw


{- | Abort execution and respond with 404 Not Found

@
#EMBED Example/Docs/App.hs findUser

#EMBED Example/Docs/App.hs userPage
@
-}
notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondNow $ Err NotFound


-- | Respond immediately with a parse error
parseError :: (Hyperbole :> es) => String -> Eff es a
parseError = respondError . ErrParse


-- | Abort execution and redirect to a 'URI'
redirect :: (Hyperbole :> es) => URI -> Eff es a
redirect = send . RespondNow . Redirect


-- | Respond with a generic view. Normally you will return a view from the page or handler instead of using this function
view :: View Body () -> Response
view =
  Response (TargetViewId mempty)
