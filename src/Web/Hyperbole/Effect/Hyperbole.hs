{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
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


-- | Run the 'Hyperbole' effect to get a response
runHyperbole
  :: Request
  -> Eff (Hyperbole : es) Response
  -> Eff es (Response, Client)
runHyperbole req = reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    pure req
  RespondNow r -> do
    throwError_ r
  GetClient -> do
    get @Client
  ModClient f -> do
    modify @Client f
 where
  runLocal :: Eff (Error Response : State Client : es) Response -> Eff es (Response, Client)
  runLocal eff = do
    let client = Client req.requestId mempty mempty
    res :: (Either Response Response, Client) <- runState client . runErrorNoCallStack @Response $ eff
    case res of
      (Left r, c) -> pure (r, c)
      (Right r, c) -> pure (r, c)
