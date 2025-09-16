module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.HyperView (HyperView (..), ViewId (..), hyperUnsafe)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View.Types


-- | Respond with the given hyperview
hyperView :: (HyperView id es) => id -> View id () -> Eff es Response
hyperView i vw = do
  let vid = TargetViewId (encodedToText $ toViewId i)
  pure $ Response vid $ hyperUnsafe i vw


respondError :: (Hyperbole :> es) => ResponseError -> Eff es a
respondError err = do
  send $ RespondNow $ Err err


respondErrorView :: (Hyperbole :> es) => Text -> View Body () -> Eff es a
respondErrorView msg vw = do
  send $ RespondNow $ Err $ ErrCustom $ ServerError msg vw


{- | Respond immediately with 404 Not Found

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


-- | Redirect immediately to the 'Url'
redirect :: (Hyperbole :> es) => URI -> Eff es a
redirect = send . RespondNow . Redirect


-- | Create a response from a given view. This is rarely used. Normally you will return a view from a handler instead
view :: View Body () -> Response
view =
  Response (TargetViewId "")
