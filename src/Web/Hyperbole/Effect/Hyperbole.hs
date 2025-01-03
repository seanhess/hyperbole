{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Web.Hyperbole.Effect.QueryData
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Effect.Session as Session


{- | In any 'load' or 'handle', you can use this Effect to get extra request information or control the response manually.

For most 'Page's, you won't need to use this effect directly. Use custom 'Route's for request info, and return 'View's to respond
-}
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondEarly :: Response -> Hyperbole m a
  ModSession :: (Session -> Session) -> Hyperbole m ()
  GetSession :: Hyperbole m Session


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
    s <- gets @HyperState (.session)
    send $ SendResponse s r
    throwError_ r
  GetSession -> do
    gets @HyperState (.session)
  ModSession f -> do
    modify @HyperState $ \st -> st{session = f st.session}
 where
  runLocal :: (Server :> es) => Eff (State HyperState : Error Response : es) a -> Eff es (Either Response (a, HyperState))
  runLocal eff = do
    -- Load the request ONCE right when we start
    r <- send LoadRequest
    let st = HyperState r (sessionFromCookies r.cookies)
    runErrorNoCallStack @Response . runState st $ eff

  combine :: (Server :> es) => Eff es (Either Response (Response, HyperState)) -> Eff es Response
  combine eff = do
    er <- eff
    case er of
      Left res ->
        -- responded early, don't need to respond again
        pure res
      Right (res, st) -> do
        send $ SendResponse st.session res
        pure res


data HyperState = HyperState
  { request :: Request
  , session :: Session
  }


-- | Lookup a session variable by keyword
session :: (FromQueryData a, Hyperbole :> es) => Text -> Eff es (Maybe a)
session k = do
  s <- send GetSession
  pure $ sessionLookup k s


-- | Set a session variable by keyword
setSession :: (ToQueryData a, Hyperbole :> es) => Text -> a -> Eff es ()
setSession k v = do
  send $ ModSession (sessionSet k v)


-- | Clear a session variable
clearSession :: (Hyperbole :> es) => Text -> Eff es ()
clearSession k = do
  send $ ModSession (sessionDel k)
