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
import Web.Hyperbole.View (toAction, toViewId)


{- ! Trigger an action for an arbitrary 'HyperView'

@
#EMBED Example.Trigger instance HyperView Controls
@
-}

{- | Trigger an action for an arbitrary 'HyperView'

@
instance 'HyperView' Controls es where
  type Require Controls = '[Targeted]

  data 'Action' Controls = TriggerMessage
    deriving (Generic, 'ViewAction')

  'update' TriggerMessage = do
    trigger Targeted $ SetMessage \"Triggered!\"
    pure controlView
@
-}
trigger :: (HyperView id es, HyperViewHandled id view, Hyperbole :> es) => id -> Action id -> Eff (Reader view : es) ()
trigger vid act = do
  send $ PushTrigger (TargetViewId $ toViewId vid) (toAction act)


{- ! Dispatch a custom javascript event. This is emitted on the current hyper view and bubbles up to the document

@
#EMBED Example.Javascript instance HyperView Message
@

@
function listenServerEvents() {
  // you can listen on document instead, the event will bubble
  Hyperbole.hyperView("Message").addEventListener("server-message", function(e) {
    alert("Server Message: " + e.detail)
  })
}
@
-}

{- | Dispatch a custom javascript event. This is emitted on the current hyper view and bubbles up to the document

@
instance 'HyperView' Message es where
  data 'Action' Message = AlertMe
    deriving (Generic, 'ViewAction')

  'update' AlertMe = do
    pushEvent \"server-message\" (\"hello\" :: Text)
    pure \"Sent 'server-message' event\"
@

@
function listenServerEvents() {
  // you can listen on document instead, the event will bubble
  Hyperbole.hyperView("Message").addEventListener("server-message", function(e) {
    alert("Server Message: " + e.detail)
  })
}
@
-}
pushEvent :: (ToJSON a, Hyperbole :> es) => Text -> a -> Eff es ()
pushEvent nm a = do
  send $ PushEvent nm (toJSON a)


{- ! Set the document title

@
#EMBED Example.Docs.Client page
@
-}

{- | Set the document title

@
page :: ('Hyperbole' :> es) => 'Page' es '[]
page = do
  pageTitle \"My 'Page' Title\"
  pure $ 'el' \"Hello World\"
@
-}
pageTitle :: (Hyperbole :> es) => Text -> Eff es ()
pageTitle t = do
  send $ ModClient $ clientSetPageTitle t
