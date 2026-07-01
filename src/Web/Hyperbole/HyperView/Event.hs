module Web.Hyperbole.HyperView.Event where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Casing (kebab)
import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.HyperView.Handled
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.View


type DelayMs = Int


event :: (ViewAction (Action id), Attributable a) => Name -> Action id -> Attributes a -> Attributes a
event nm a = att (eventName nm) (encodedToText $ toAction a)


eventName :: Text -> Name
eventName t = "data-on" <> t


{- ! Send the action after N milliseconds. Can be used to implement lazy loading or polling.

@
#EMBED Example.Concurrency.LazyLoading viewTaskLoad
@
-}


{- | Send the action after N milliseconds. Can be used to implement lazy loading or polling.

@
viewTaskLoad :: 'View' LazyData ()
viewTaskLoad = do
  -- 100ms after rendering, get the details
  'el' @ onLoad Details 100 ~ bg GrayLight . textAlign AlignCenter $ do
    text \"...\"
@
-}
onLoad :: (ViewAction (Action id), Attributable a) => Action id -> DelayMs -> Attributes a -> Attributes a
onLoad a delay = do
  event "load" a . att "data-delay" (cs $ show delay)


onClick :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onClick = event "click"


onDblClick :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onDblClick = event "dblclick"


onMouseEnter :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onMouseEnter = event "mouseenter"


onMouseLeave :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onMouseLeave = event "mouseleave"


{- | Run an action when the user types into an 'input' or 'textarea'.

WARNING: a short delay can result in poor performance. It is not recommended to set the 'value' of the input

> input (onInput OnSearch) 250 id
-}
onInput :: (ViewAction (Action id), Attributable a) => Action id -> DelayMs -> Attributes a -> Attributes a
onInput a delay = do
  att (eventName "input") (encodedToText $ toAction a) . att "data-delay" (cs $ show delay)


onChange :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onChange a = do
  att (eventName "change") (encodedToText $ toAction a)


onSubmit :: (ViewAction (Action id), Attributable a) => Action id -> Attributes a -> Attributes a
onSubmit = event "submit"


onKeyDown :: (ViewAction (Action id), Attributable a) => Key -> Action id -> Attributes a -> Attributes a
onKeyDown key = do
  event ("keydown-" <> keyDataAttribute key)


onKeyUp :: (ViewAction (Action id), Attributable a) => Key -> Action id -> Attributes a -> Attributes a
onKeyUp key = do
  event ("keyup-" <> keyDataAttribute key)


keyDataAttribute :: Key -> Text
keyDataAttribute = cs . kebab . showKey
 where
  showKey (OtherKey t) = cs t
  showKey k = show k


-- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
data Key
  = ArrowDown
  | ArrowUp
  | ArrowLeft
  | ArrowRight
  | Enter
  | Space
  | Escape
  | Alt
  | CapsLock
  | Control
  | Fn
  | Meta
  | Shift
  | OtherKey Text
  deriving (Show, Read)


-- | Internal
dataTarget :: (ViewId id, Attributable a) => id -> Attributes a -> Attributes a
dataTarget = att "data-target" . encodedToText . toViewId


{- ! Allow inputs to trigger actions for a different view

@
#EMBED Example.Trigger targetView
@
-}


{- | Allow inputs to trigger actions for a different view

@
targetView :: 'View' Controls ()
targetView = do
  target Targeted () $ do
    'button' (SetMessage \"Targeted!\") ~ btn $ \"Target SetMessage\"
@
-}
target :: forall id ctx. (HyperViewHandled id ctx, ViewId id) => id -> ViewState id -> View id () -> View ctx ()
target newId st view = do
  runViewContext newId st $ do
    view @ dataTarget newId
