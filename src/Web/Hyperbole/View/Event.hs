{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.View.Event where

import Data.Text (Text, pack)
import Data.Text qualified as T
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
onLoad :: (ViewId id, ViewAction (Action id)) => Action id -> DelayMs -> Mod id
onLoad a delay = do
  att "data-on-load" (toAction a) . att "data-delay" (pack $ show delay)


onClick :: (ViewId id, ViewAction (Action id)) => Action id -> Mod id
onClick a = do
  att "data-on-click" (toAction a)


onInput :: (ViewId id, ViewAction (Action id)) => (Text -> Action id) -> DelayMs -> Mod id
onInput a delay = do
  att "data-on-input" (toActionInput a) . att "data-delay" (pack $ show delay)


onSubmit :: (ViewId id, ViewAction (Action id)) => Action id -> Mod id
onSubmit act = do
  att "data-on-submit" (toAction act)


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
