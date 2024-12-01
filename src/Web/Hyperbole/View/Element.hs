module Web.Hyperbole.View.Element where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Web.Hyperbole.Component
import Web.Hyperbole.Route (Route (..), routeUrl)
import Web.Hyperbole.View.Event (DelayMs)
import Web.Hyperbole.View.Target (dataTarget)
import Web.View hiding (Query, Segment, button, cssResetEmbed, form, input, label)


{- | \<button\> HTML tag which sends the action when pressed

> button SomeAction (border 1) "Click Me"
-}
button :: (ViewId id, IsAction (Action id)) => Action id -> Mod -> View id () -> View id ()
button a f cd = do
  c <- context
  tag "button" (att "data-on-click" (toAction a) . dataTarget c . f) cd


{- | Type-safe dropdown. Sends (opt -> Action id) when selected. The selection predicate (opt -> Bool) controls which option is selected. See [Example.Contacts](https://github.com/seanhess/hyperbole/blob/main/example/Example/Contacts.hs)

@
data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  deriving (Generic, Param)

allContactsView :: Maybe Filter -> View Contacts ()
allContactsView fil = do
  row (gap 10) $ do
    el (pad 10) "Filter: "
    dropdown Reload (== fil) id $ do
      option Nothing ""
      option (Just Active) "Active!"
      option (Just Inactive) \"Inactive\"
  ...
@
-}
dropdown
  :: (ViewId id)
  => (opt -> Action id)
  -> (opt -> Bool) -- check if selec
  -> Mod
  -> View (Option opt id (Action id)) ()
  -> View id ()
dropdown act isSel f options = do
  c <- context
  tag "select" (att "data-on-change" "" . dataTarget c . f) $ do
    addContext (Option act isSel) options


-- | An option for a 'dropdown'. First argument is passed to (opt -> Action id) in the 'dropdown', and to the selected predicate
option
  :: (ViewId id, Eq opt, IsAction (Action id))
  => opt
  -> View (Option opt id (Action id)) ()
  -> View (Option opt id (Action id)) ()
option opt cnt = do
  os <- context
  tag "option" (att "value" (toAction (os.toAction opt)) . selected (os.selected opt)) cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: Bool -> Mod
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option opt id action = Option
  { toAction :: opt -> action
  , selected :: opt -> Bool
  }


-- | A live search field
search :: (ViewId id, IsAction (Action id)) => (Text -> Action id) -> DelayMs -> Mod -> View id ()
search onInput delay f = do
  c <- context
  tag "input" (att "data-on-input" (toActionInput onInput) . att "data-delay" (pack $ show delay) . dataTarget c . f) none


-- | Serialize a constructor that expects a single 'Text', like `data MyAction = GoSearch Text`
toActionInput :: (IsAction (Action a)) => (Text -> Action a) -> Text
toActionInput con =
  -- remove the ' ""' at the end of the constructor
  T.dropEnd 3 $ toAction $ con ""


{- | A hyperlink to another route

>>> route (User 100) id "View User"
<a href="/user/100">View User</a>
-}
route :: (Route a) => a -> Mod -> View c () -> View c ()
route r = link (routeUrl r)
