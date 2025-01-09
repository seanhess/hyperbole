{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Web.Hyperbole.Data.QueryData (queryData)
import Web.Hyperbole.Effect.Server


-- | The 'Hyperbole' 'Effect' allows you to access information in the 'Request', manually 'respondEarly', and manipulate the Client 'session' and 'query'.
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondEarly :: Response -> Hyperbole m a
  ModClient :: (Client -> Client) -> Hyperbole m ()
  GetClient :: Hyperbole m Client


type instance DispatchOf Hyperbole = 'Dynamic


-- | Run the 'Hyperbole' effect to 'Server'
runHyperbole
  :: (Server :> es)
  => Eff (Hyperbole : es) Response
  -> Eff es Response
runHyperbole = fmap combine $ reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    gets @HyperState (.request)
  RespondEarly r -> do
    s <- gets @HyperState (.client)
    send $ SendResponse s r
    throwError_ r
  GetClient -> do
    gets @HyperState (.client)
  ModClient f -> do
    modify @HyperState $ \st -> st{client = f st.client}
 where
  runLocal :: (Server :> es) => Eff (State HyperState : Error Response : es) a -> Eff es (Either Response (a, HyperState))
  runLocal eff = do
    -- Load the request ONCE right when we start
    r <- send LoadRequest
    let client = Client mempty (queryData $ queryParams r)
    let st = HyperState r client
    runErrorNoCallStack @Response . runState st $ eff

  queryParams request =
    filter (not . isSystemParam) request.query

  isSystemParam (key, _) =
    "hyp-" `BS.isPrefixOf` key

  combine :: (Server :> es) => Eff es (Either Response (Response, HyperState)) -> Eff es Response
  combine eff = do
    er <- eff
    case er of
      Left res ->
        -- responded early, don't need to respond again
        pure res
      Right (res, st) -> do
        send $ SendResponse st.client res
        pure res


data HyperState = HyperState
  { request :: Request
  , client :: Client
  }
