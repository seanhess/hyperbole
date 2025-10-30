module Web.Hyperbole.Effect.Client where

import Data.Aeson
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.Types.Client (clientSetPageTitle)
import Web.Hyperbole.Types.Event


{- | Trigger an action for an arbitrary 'HyperView'

#EXAMPLE /advanced

@
#EMBED Example/Page/Advanced.hs instance HyperView Controls
@
-}
trigger :: (HyperView id es, HyperViewHandled id view, Hyperbole :> es) => id -> Action id -> Eff (Reader view : es) ()
trigger vid act = do
  send $ TriggerAction (TargetViewId $ toViewId vid) (toAction act)


{- | Dispatch a custom javascript event. This is emitted on the current hyper view and bubbles up to the document

#EXAMPLE /javascript

@
#EMBED Example/Page/Javascript.hs instance HyperView Message
@

@
#EMBED static/custom.js function listenServerEvents
@
-}
pushEvent :: (ToJSON a, Hyperbole :> es) => Text -> a -> Eff es ()
pushEvent nm a = do
  send $ TriggerEvent nm (toJSON a)


{- | Set the document title

@
#EMBED Example/Docs/Client.hs page
@
-}
pageTitle :: (Hyperbole :> es) => Text -> Eff es ()
pageTitle t = do
  send $ ModClient $ clientSetPageTitle t
