{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.View.Event where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Casing (kebab)
import Web.Hyperbole.HyperView
import Web.View (Mod, View, addContext, att, parent)
import Web.View.Types (Content (Node), Element (..))
import Web.View.View (viewModContents)


type DelayMs = Int


{- | Send the action after N milliseconds. Can be used to implement lazy loading or polling

@
pollMessageView :: Text -> 'View' Message ()
pollMessageView m = do
  onLoad LoadMessage 1000 $ do
    'el' 'bold' "Current Message. Reloading in 1s"
    'el_' ('text' m)
@
-}
onLoad :: (ViewAction (Action id)) => Action id -> DelayMs -> Mod id
onLoad a delay = do
  att "data-on-load" (toAction a) . att "data-delay" (cs $ show delay)


onClick :: (ViewAction (Action id)) => Action id -> Mod id
onClick a = do
  att "data-on-click" (toAction a)


onDblClick :: (ViewAction (Action id)) => Action id -> Mod id
onDblClick a = do
  att "data-on-dblclick" (toAction a)


{- | Run an action when the user types into an 'input' or 'textarea'.

WARNING: a short delay can result in poor performance. It is not recommended to set the 'value' of the input

> input (onInput OnSearch) 250 id
-}
onInput :: (ViewAction (Action id)) => (Text -> Action id) -> DelayMs -> Mod id
onInput a delay = do
  att "data-on-input" (toActionInput a) . att "data-delay" (cs $ show delay)


onSubmit :: (ViewAction (Action id)) => Action id -> Mod id
onSubmit act = do
  att "data-on-submit" (toAction act)


onKeyDown :: (ViewAction (Action id)) => Key -> Action id -> Mod id
onKeyDown key act = do
  att ("data-on-keydown-" <> keyDataAttribute key) (toAction act)


onKeyUp :: (ViewAction (Action id)) => Key -> Action id -> Mod id
onKeyUp key act = do
  att ("data-on-keyup-" <> keyDataAttribute key) (toAction act)


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


-- addDataKey :: Key -> Mod c
-- addDataKey k atts =
--   atts{other = M.alter merge "data-key" atts.other}
--  where
--   merge :: Maybe AttValue -> Maybe AttValue
--   merge Nothing = pure $ toKeyValue k
--   merge (Just keys) = pure $ toKeyValue k <> " " <> keys

-- let keyAtt = "data-" <> keyDataAttribute key

-- | Serialize a constructor that expects a single 'Text', like `data MyAction = GoSearch Text`
toActionInput :: (ViewAction a) => (Text -> a) -> Text
toActionInput con =
  -- remove the ' ""' at the end of the constructor
  T.dropEnd 3 $ toAction $ con ""


{- | Apply a Mod only when a request is in flight

@
myView = do
  el (hide . onRequest flexCol) 'el_' "Loading..."
  el (onRequest hide) "Loaded"
@
-}
onRequest :: Mod id -> Mod id
onRequest f = do
  parent "hyp-loading" f


-- | Internal
dataTarget :: (ViewId a) => a -> Mod x
dataTarget = att "data-target" . toViewId


{- | Trigger actions for another view. They will update the view specified

> otherView :: View OtherView ()
> otherView = do
>   el_ "This is not a message view"
>   button OtherAction id "Do Something"
>
>   target (Message 2) $ do
>     el_ "Now we can trigger a MessageAction which will update our Message HyperView, not this one"
>     button ClearMessage id "Clear Message #2"
-}
target :: forall id ctx. (HyperViewHandled id ctx, ViewId id) => id -> View id () -> View ctx ()
target newId view = do
  addContext newId $ do
    view
    viewModContents (fmap addDataTarget)
 where
  addDataTarget :: Content -> Content
  addDataTarget = \case
    Node elm ->
      Node $
        let atts = elm.attributes
         in elm{attributes = dataTarget newId atts}
    cnt -> cnt
