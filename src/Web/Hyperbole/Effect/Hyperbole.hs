{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..))
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Session as Session


{- | In any 'load' or 'handle', you can use this Effect to get extra request information or control the response manually.

For most 'Page's, you won't need to use this effect directly. Use custom 'Route's for request info, and return 'View's to respond
-}
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondEarly :: Response -> Hyperbole m a
  SetSession :: (ToHttpApiData a) => Text -> a -> Hyperbole m ()
  DelSession :: Text -> Hyperbole m ()
  GetSession :: (FromHttpApiData a) => Text -> Hyperbole m (Maybe a)


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
  SetSession k a -> do
    modify $ \st -> st{session = sessionSet k a st.session} :: HyperState
  DelSession k -> do
    modify $ \st -> st{session = sessionDel k st.session} :: HyperState
  GetSession k -> do
    s <- gets @HyperState (.session)
    pure $ sessionLookup k s
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


{- | Lookup a session variable by keyword

> load $ do
>   tok <- session "token"
>   ...
-}
session :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es (Maybe a)
session k = send $ GetSession k


{- | Set a session variable by keyword

> load $ do
>   t <- reqParam "token"
>   setSession "token" t
>   ...
-}
setSession :: (Hyperbole :> es, ToHttpApiData a) => Text -> a -> Eff es ()
setSession k v = send $ SetSession k v


-- | Clear the user's session
clearSession :: (Hyperbole :> es) => Text -> Eff es ()
clearSession k = send $ DelSession k
