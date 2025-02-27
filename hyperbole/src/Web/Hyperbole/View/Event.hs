{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.View.Event where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Casing (kebab)
import Web.Hyperbole.HyperView
import Web.View (Mod, View, addContext, att, parent)
import Web.View.Types (Content (Node), Element (..))
import Web.View.View (viewModContents)


type DelayMs = Int


{- | Send the action after N milliseconds. Can be used to implement lazy loading or polling. See [Example.Page.Concurrent](https://docs.hyperbole.live/concurrent)

@
#EMBED Example/Page/LazyLoading.hs viewTaskLoad
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


-- | Serialize a constructor that expects a single 'Text', like `data MyAction = GoSearch Text`
toActionInput :: (ViewAction a) => (Text -> a) -> Text
toActionInput con =
  let marker = "%HYP-INP%"
   in toAction $ con marker


{- | Apply a Mod only when a request is in flight. See [Example.Page.Contact](https://docs.hyperbole.live/contacts/1)

@
#EMBED Example/Page/Contact.hs contactEditView
@
-}
onRequest :: Mod id -> Mod id
onRequest f = do
  parent "hyp-loading" f


-- | Internal
dataTarget :: (ViewId a) => a -> Mod x
dataTarget = att "data-target" . toViewId


-- | Allow inputs to trigger actions for a different view
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
