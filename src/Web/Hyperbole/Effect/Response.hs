module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.HyperView (ConcurrencyValue (..), HyperView (..), hyperUnsafe)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View


-- | Respond with the given hyperview
hyperView :: (HyperView id es, ToEncoded (ViewState id)) => id -> ViewState id -> View id () -> Eff es Response
hyperView i st vw = do
  let vid = TargetViewId (toViewId i)
  pure $ Response $ ViewUpdate vid $ renderBody $ hyperUnsafe i st vw


pushUpdate :: (Hyperbole :> es, ViewId id, ToEncoded (ViewState id), ConcurrencyValue (Concurrency id)) => View id () -> Eff (Reader id : State (ViewState id) : es) ()
pushUpdate vw = do
  i <- viewId
  st <- get
  pushUpdateTo i st vw


pushUpdateTo :: (Hyperbole :> es, ViewId id, ToEncoded (ViewState id), ConcurrencyValue (Concurrency id)) => id -> ViewState id -> View id () -> Eff es ()
pushUpdateTo i st vw = do
  send $ PushUpdate $ ViewUpdate (TargetViewId $ toViewId i) $ renderBody $ hyperUnsafe i st vw


-- | Abort execution and respond with an error
respondError :: (Hyperbole :> es) => ResponseError -> Eff es a
respondError err = do
  send $ RespondNow $ Err err


-- | Abort execution and respond with an error view
respondErrorView :: (Hyperbole :> es) => Text -> View () () -> Eff es a
respondErrorView msg vw = do
  send $ RespondNow $ Err $ ErrCustom $ ServerError msg $ renderBody vw


{- | Abort execution and respond with 404 Not Found

@
#EMBED Example.Docs.App findUser

#EMBED Example.Docs.App userPage
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
view :: View () () -> Response
view v =
  Response $ ViewUpdate (TargetViewId mempty) (renderBody v)
