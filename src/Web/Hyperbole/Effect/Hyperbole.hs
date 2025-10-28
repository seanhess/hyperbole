{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View)


-- | The 'Hyperbole' 'Effect' allows you to access information in the 'Request', manually respond, and manipulate the Client 'session' and 'query'.
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondNow :: Response -> Hyperbole m a
  PushUpdate :: TargetViewId -> View Body () -> Hyperbole m ()
  ModClient :: (Client -> Client) -> Hyperbole m ()
  GetClient :: Hyperbole m Client
  -- TODO: this should actually execute the other view, and send the response to the client
  TriggerAction :: TargetViewId -> Encoded -> Hyperbole m ()
  TriggerEvent :: Text -> Value -> Hyperbole m ()


type instance DispatchOf Hyperbole = 'Dynamic


data Remote
  = RemoteAction TargetViewId Encoded
  | RemoteEvent Text Value


runHyperboleLocal :: Request -> Eff (Error Response : State Client : Writer [Remote] : es) Response -> Eff es (Response, Client, [Remote])
runHyperboleLocal req eff = do
  ((eresp, client'), rmts) <- runWriter @[Remote] . runState (emptyClient req.requestId) . runErrorNoCallStack @Response $ eff
  pure (either id id eresp, client', rmts)
 where
  emptyClient :: RequestId -> Client
  emptyClient requestId =
    Client
      { requestId
      , session = mempty
      , query = mempty
      , pageTitle = Nothing
      }
