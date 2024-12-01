module Web.Hyperbole.View.Event where

import Data.Text (pack)
import Web.Hyperbole.Component (IsAction (..), ViewId)
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Target
import Web.View (View, att, context, el, flexCol, hide, parent)


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
onLoad :: (IsAction (Action id), ViewId id) => Action id -> DelayMs -> View id () -> View id ()
onLoad a delay initContent = do
  c <- context
  el (att "data-on-load" (toAction a) . att "data-delay" (pack $ show delay) . dataTarget c) initContent


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
