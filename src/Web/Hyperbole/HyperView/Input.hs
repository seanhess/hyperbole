module Web.Hyperbole.HyperView.Input where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as A
import Data.String.Conversions (cs)
import Data.Text (Text)
import Web.Atomic.Types
import Web.Hyperbole.HyperView.Event (DelayMs, onChange, onClick, onInput)
import Web.Hyperbole.HyperView.Types (HyperView (..), ViewAction (..))
import Web.Hyperbole.Route (Route (..), routeUri)
import Web.Hyperbole.View


{- | \<button\> HTML tag which sends the action when pressed

> button SomeAction (border 1) "Click Me"
-}
button :: (ViewAction (Action id)) => Action id -> View id () -> View id ()
button action cnt = do
  tag "button" cnt @ onClick action


-- tag "button" @ att "whatber" "asdf" $ cnt

-- {- | \<input type="checkbox"\> which toggles automatically
--
-- > toggle True SetIsSelected id
-- -}
-- toggle :: (ViewAction (Action id)) => Bool -> (Bool -> Action id) -> Mod id -> View id ()
-- toggle isSelected clickAction f = do
--   tag "input" (att "type" "checkbox" . checked isSelected . onClick (clickAction (not isSelected)) . f) none

{- | Type-safe dropdown. Sends (opt -> Action id) when selected. The selection predicate (opt -> Bool) controls which option is selected. See [Example.Page.Filter](https://docs.hyperbole.live/filter)

@
#EMBED Example/Page/Filter.hs familyDropdown
@
-}
dropdown
  :: (ViewAction (Action id))
  => (opt -> Action id)
  -> (opt -> Bool) -- check if selec
  -> View (Option opt id) ()
  -> View id ()
dropdown act isSel options = do
  tag "select" @ onChange act $ do
    addContext (Option isSel) options


-- | An option for a 'dropdown'. First argument is passed to (opt -> Action id) in the 'dropdown', and to the selected predicate
option
  :: (ViewAction (Action id), Eq opt, ToJSON opt)
  => opt
  -> View (Option opt id) ()
  -> View (Option opt id) ()
option opt cnt = do
  os <- context
  tag "option" @ att "value" (cs $ A.encode opt) @ selected (os.selected opt) $ cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: (Attributable h) => Bool -> Attributes h -> Attributes h
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option opt id = Option
  { selected :: opt -> Bool
  }


-- | A live search field
search :: (ViewAction (Action id)) => (Text -> Action id) -> DelayMs -> View id ()
search go delay = do
  tag "input" none @ onInput go delay


{- | Set checkbox = checked via the client (VDOM doesn't work)
designed for input, which has no children
-}
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
