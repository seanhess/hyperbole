module Web.Hyperbole.Response
  ( Response (..)
  , ResponseError (..)
  , TargetViewId (..)
  )
where

import Data.Text (Text)
import Web.Hyperbole.Event (Event)
import Web.View


-- | Serialized ViewId
newtype TargetViewId = TargetViewId Text


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
