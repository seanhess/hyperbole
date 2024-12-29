{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Request where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server
import Web.View


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es [Segment]
reqPath = (.path) <$> request


{- | Return the request body as a Web.FormUrlEncoded.Form

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Hyperbole :> es) => Eff es Form
formBody = do
  b <- (.body) <$> request
  let ef = urlDecodeForm b
  -- not going to work. we need a way to `throwError` or it doesn't work...
  either (send . RespondEarly . Err . ErrParse) pure ef


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
reqParams :: (Hyperbole :> es) => Eff es Query
reqParams = (.query) <$> request


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
reqParam :: forall a es. (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
reqParam p = do
  q <- reqParams
  (er :: Either Response a) <- pure $ do
    mv <- require $ List.lookup (encodeUtf8 p) q
    v <- require mv
    first (Err . ErrParam) $ parseQueryParam (decodeUtf8 v)
  case er of
    Left e -> send $ RespondEarly e
    Right a -> pure a
 where
  require :: Maybe x -> Either Response x
  require Nothing = Left $ Err $ ErrParam $ "Missing: " <> p
  require (Just a) = pure a

{- | Maybe version of 'reqParam'

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
      mbToken <- reqParamMaybe "token"
      sideEffectUsingToken $ fromMaybe "default" mbToken
      pure myPageView
@
-}
reqParamMaybe :: forall a es. (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es (Maybe a)
reqParamMaybe p = do
  q <- reqParams
  pure $ lookup (encodeUtf8 p) q >>= \case
    Nothing -> Nothing
    Just v -> either (const Nothing) Just $ parseQueryParam (decodeUtf8 v)
  
-- | Lookup the query param in the 'Query'
lookupParam :: BS.ByteString -> Query -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q


-- | Whether the param is present or not
hasParam :: BS.ByteString -> Query -> Bool
hasParam p q =
  isJust $ lookup p q
