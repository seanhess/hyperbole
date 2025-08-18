{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response


-- | The 'Hyperbole' 'Effect' allows you to access information in the 'Request', manually respond, and manipulate the Client 'session' and 'query'.
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondNow :: Response -> Hyperbole m a
  ModClient :: (Client -> Client) -> Hyperbole m ()
  GetClient :: Hyperbole m Client
  -- TODO: this should actually execute the other view, and send the response to the client
  TriggerAction :: TargetViewId -> Encoded -> Hyperbole m ()
  TriggerEvent :: Text -> Value -> Hyperbole m ()


type instance DispatchOf Hyperbole = 'Dynamic


data Remote
  = RemoteAction TargetViewId Encoded
  | RemoteEvent Text Value


-- | Run the 'Hyperbole' effect to get a response
runHyperbole
  :: Request
  -> Eff (Hyperbole : es) Response
  -> Eff es (Response, Client, [Remote])
runHyperbole req = reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    pure req
  RespondNow r -> do
    throwError_ r
  GetClient -> do
    get @Client
  ModClient f -> do
    modify @Client f
  TriggerAction vid act -> do
    tell [RemoteAction vid act]
  TriggerEvent name dat -> do
    tell [RemoteEvent name dat]
 where
  runLocal :: Eff (Error Response : State Client : Writer [Remote] : es) Response -> Eff es (Response, Client, [Remote])
  runLocal eff = do
    ((eresp, client'), rmts) <- runWriter @[Remote] . runState (emptyClient req.requestId) . runErrorNoCallStack @Response $ eff
    pure (either id id eresp, client', rmts)

  emptyClient :: RequestId -> Client
  emptyClient requestId =
    Client
      { requestId
      , session = mempty
      , query = mempty
      , pageTitle = Nothing
      }
