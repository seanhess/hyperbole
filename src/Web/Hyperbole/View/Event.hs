module Web.Hyperbole.View.Event where

import Data.Text (pack)
import Web.Hyperbole.HyperView
import Web.View (Mod, View, att, el, flexCol, hide, parent)


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


{- | Give visual feedback when an action is in-flight.

@
myView = do
  onRequest loadingIndicator $ do
    'el_' \"Loaded\"
  where
    loadingIndicator = 'el_' "Loading..."
@
-}
onRequest :: View id () -> View id () -> View id ()
onRequest a b = do
  el (parent "hyp-loading" flexCol . hide) a
  el (parent "hyp-loading" hide . flexCol) b


-- | Internal
dataTarget :: (ViewId a) => a -> Mod a
dataTarget = att "data-target" . toViewId
