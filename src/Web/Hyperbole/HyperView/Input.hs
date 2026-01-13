module Web.Hyperbole.HyperView.Input where

import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Atomic.Types
import Web.Hyperbole.Data.Param (FromParam, ParamValue (..), ToParam (..))
import Web.Hyperbole.HyperView.Event (DelayMs, onChange, onClick, onInput)
import Web.Hyperbole.HyperView.Types (HyperView (..))
import Web.Hyperbole.Route (Route (..), routeUri)
import Web.Hyperbole.View


{- | \<button\> HTML tag which sends the action when pressed

@
#EMBED Example.Simple messageView
@
-}
button :: (ViewAction (Action id)) => Action id -> View id () -> View id ()
button action cnt = do
  tag "button" cnt @ onClick action


{- | Type-safe dropdown. Sends (opt -> Action id) when selected. The default will be selected.

#EXAMPLE /data/filter

@
#EMBED App.Page.DataLists.Filter familyDropdown
@
-}
dropdown
  :: forall opt id
   . (ViewAction (Action id))
  => (opt -> Action id)
  -> opt -- default option
  -> View (Option id opt) ()
  -> View id ()
dropdown act defOpt options = do
  st :: ViewState id <- viewState
  i :: id <- viewId
  tag "select" @ onChange act $ do
    runViewContext (Option i defOpt) st options


-- | An option for a 'dropdown' or 'select'
option
  :: forall opt id
   . (ViewAction (Action id), Eq opt, ToParam opt)
  => opt
  -> Text
  -> View (Option id opt) ()
option opt cnt = do
  os :: Option id opt <- viewId
  tag "option" @ att "value" (toParam opt).value @ selected (os.defaultOption == opt) $ text cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: (Attributable h) => Bool -> Attributes h -> Attributes h
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option id opt = Option
  { id :: id
  , defaultOption :: opt
  }
  deriving (Generic)


instance (ToParam id, ToParam opt, FromParam id, FromParam opt) => ViewId (Option id opt) where
  type ViewState (Option id opt) = ViewState id


{- | A live search field. Set a DelayMs to avoid hitting the server on every keystroke

@
#EMBED Example.Errors viewSearchUsers
@
-}
search :: (ViewAction (Action id)) => (Text -> Action id) -> DelayMs -> View id ()
search go delay = do
  tag "input" none @ onInput go delay


-- | Set checkbox = checked via the client (VDOM doesn't work)
checked :: (Attributable a) => Bool -> Attributes a -> Attributes a
checked c =
  att "data-checked" (cs $ show c)
    . if c then att "checked" "" else id


{- | A hyperlink to another route

>>> route (User 100) id "View User"
<a href="/user/100">View User</a>
-}
route :: (Route a) => a -> View c () -> View c ()
route r = link (routeUri r)
