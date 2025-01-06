module Web.Hyperbole.Effect.Query where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.QueryData (FromParam (..), FromQuery (..), QueryData (..), ToParam (..), ToQuery (..))
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Client (..), Response (..), ResponseError (..))
import Prelude


{- | Require a given parameter from the 'Query' arguments

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    token <- reqParam "token"
    sideEffectUsingToken token
    pure myPageView
@
-}
param :: forall a es. (Hyperbole :> es, FromParam a) => Text -> Eff es a
param p = do
  q <- queryParams
  case QueryData.require p q of
    Left e -> send $ RespondEarly $ Err $ ErrQuery e
    Right a -> pure a


{- | Maybe version of param

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
      mbToken <- lookupParam "token"
      sideEffectUsingToken $ fromMaybe "default" mbToken
      pure myPageView
@
-}
lookupParam :: forall a es. (Hyperbole :> es, FromParam a) => Text -> Eff es (Maybe a)
lookupParam p = do
  QueryData.lookup p <$> queryParams


setParam :: (Hyperbole :> es, ToParam a) => Text -> a -> Eff es ()
setParam key a = do
  modifyQuery (QueryData.insert key a)


deleteParam :: (Hyperbole :> es) => Text -> Eff es ()
deleteParam key = do
  modifyQuery (QueryData.delete key)


query :: (Hyperbole :> es, FromQuery a) => Eff es a
query = do
  q <- queryParams
  case parseQuery q of
    Left e -> send $ RespondEarly $ Err $ ErrQuery $ "Query Parse " <> e <> " from " <> cs (show q)
    Right a -> pure a


-- replace the query with this
setQuery :: (Hyperbole :> es, ToQuery a) => a -> Eff es ()
setQuery a = do
  modifyQuery (const $ toQuery a)


{- | Return the entire 'Query'

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    q <- reqParams
    case 'lookupParam' "token" q of
      Nothing -> pure $ errorView "Missing Token in Query String"
      Just t -> do
        sideEffectUsingToken token
        pure myPageView
@
-}
queryParams :: (Hyperbole :> es) => Eff es QueryData
queryParams = do
  -- TODO: should be loaded when we start from the request
  (.query) <$> send GetClient


modifyQuery :: (Hyperbole :> es) => (QueryData -> QueryData) -> Eff es ()
modifyQuery f =
  send $ ModClient $ \client ->
    Client{query = f client.query, session = client.session}

-- TODO: get a single param
-- TODO: get an entire set of params
-- TODO: lookup a param
