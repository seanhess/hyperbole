module Web.Hyperbole.View.Element where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Web.Hyperbole.HyperView (HyperView (..), ViewAction (..))
import Web.Hyperbole.Route (Route (..), routeUrl)
import Web.Hyperbole.View.Event (DelayMs, onClick, onInput)
import Web.View hiding (Query, Segment, button, cssResetEmbed, form, input, label)


{- | \<button\> HTML tag which sends the action when pressed

> button SomeAction (border 1) "Click Me"
-}
button :: (ViewAction (Action id)) => Action id -> Mod id -> View id () -> View id ()
button action f cd = do
  tag "button" (onClick action . f) cd


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
  -> Mod id
  -> View (Option opt id (Action id)) ()
  -> View id ()
dropdown act isSel f options = do
  tag "select" (att "data-on-change" "" . f) $ do
    addContext (Option act isSel) options


-- | An option for a 'dropdown'. First argument is passed to (opt -> Action id) in the 'dropdown', and to the selected predicate
option
  :: (ViewAction (Action id), Eq opt)
  => opt
  -> View (Option opt id (Action id)) ()
  -> View (Option opt id (Action id)) ()
option opt cnt = do
  os <- context
  tag "option" (att "value" (toAction (os.toAction opt)) . selected (os.selected opt)) cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: Bool -> Mod id
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option opt id action = Option
  { toAction :: opt -> action
  , selected :: opt -> Bool
  }


-- | A live search field
search :: (ViewAction (Action id)) => (Text -> Action id) -> DelayMs -> Mod id -> View id ()
search go delay f = do
  tag "input" (onInput go delay . f) none


{- | A hyperlink to another route

>>> route (User 100) id "View User"
<a href="/user/100">View User</a>
-}
route :: (Route a) => a -> Mod c -> View c () -> View c ()
route r = link (routeUrl r)


checked :: Bool -> Mod id
checked c =
  att "data-checked" (cs $ show c)
    . if c then att "checked" "" else id
