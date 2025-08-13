{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Web.Hyperbole.Server.Types (Server (..))
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response


-- | The 'Hyperbole' 'Effect' allows you to access information in the 'Request', manually respond, and manipulate the Client 'session' and 'query'.
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondNow :: Response -> Hyperbole m a
  ModClient :: (Client -> Client) -> Hyperbole m ()
  GetClient :: Hyperbole m Client


type instance DispatchOf Hyperbole = 'Dynamic


-- | Run the 'Hyperbole' effect to 'Server'
runHyperbole
  :: (Server :> es)
  => Request
  -> Eff (Hyperbole : es) Response
  -> Eff es Response
runHyperbole req = fmap combine $ reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    pure req
  RespondNow r -> do
    s <- get @Client
    send $ SendResponse s r
    throwError_ r
  GetClient -> do
    get @Client
  ModClient f -> do
    modify @Client f
 where
  runLocal :: (Server :> es) => Eff (State Client : Error Response : es) a -> Eff es (Either Response (a, Client))
  runLocal eff = do
    let client = Client req.requestId mempty mempty
    runErrorNoCallStack @Response . runState client $ eff

  combine :: (Server :> es) => Eff es (Either Response (Response, Client)) -> Eff es Response
  combine eff = do
    er <- eff
    case er of
      Left res ->
        -- responded via RespondNow, don't need to respond again
        pure res
      Right (res, client) -> do
        send $ SendResponse client res
        pure res
