module Web.Hyperbole.Effect.Response where

import Data.Text (Text)
import Web.View (Url, View)


{- | Valid responses for a 'Hyperbole' effect. Use 'notFound', etc instead. Reminds you to use 'load' in your 'Page'

> myPage :: (Hyperbole :> es) => Page es Response
> myPage = do
>   -- compiler error: () does not equal Response
>   pure ()
-}
data Response
  = Response TargetViewId (View () ())
  | NotFound
  | Redirect Url
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrParam Text
  | ErrOther Text
  | ErrNotHandled (Event Text Text)
  | ErrAuth
  deriving (Show)


-- | Serialized ViewId
newtype TargetViewId = TargetViewId Text


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action
