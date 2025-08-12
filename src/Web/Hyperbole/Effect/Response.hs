module Web.Hyperbole.Effect.Response where


-- import Data.Text (Text)
-- import Effectful
-- import Effectful.Dispatch.Dynamic
-- import Web.Hyperbole.Data.Encoded
-- import Web.Hyperbole.Data.URI
-- import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
-- import Web.Hyperbole.Effect.Server (Response (..))
-- import Web.Hyperbole.HyperView (HyperView (..), hyperUnsafe)
-- import Web.Hyperbole.Types.Error (ResponseError (..))
-- import Web.Hyperbole.Types.Event (TargetViewId (..))
-- import Web.Hyperbole.Types.ViewId (ViewId (..))
-- import Web.Hyperbole.View.Types

-- -- | Respond with the given view immediately, stopping execution
-- respondView :: (Hyperbole :> es, HyperView id es) => id -> View id () -> Eff es a
-- respondView i vw = do
--   let vid = TargetViewId (encodedToText $ toViewId i)
--   let res = Response vid $ hyperUnsafe i vw
--   send $ RespondNow res
--
--
-- respondError :: (Hyperbole :> es) => ResponseError -> Eff es a
-- respondError err = do
--   send $ RespondNow $ Err err
--
--
-- respondErrorView :: (Hyperbole :> es) => Text -> View () () -> Eff es a
-- respondErrorView msg vw = do
--   send $ RespondNow $ Err $ ErrCustom msg vw

-- -- | Create a response from a given view. This is rarely used. Normally you will return a view from a handler instead
-- viewResponse :: View () () -> Response
-- viewResponse =
--   Response (TargetViewId "")
