module Web.Hyperbole.Effect.Request where

import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.Data.URI (Path (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Response (parseError)
import Web.Hyperbole.HyperView.Input (InputValue (..))
import Web.Hyperbole.Types.Request


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es Path
reqPath = (.path) <$> request


{- | Return the request body as a Web.FormUrlEncoded.Form

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Hyperbole :> es) => Eff es Form
formBody = do
  b <- (.body) <$> request
  case urlDecodeForm b of
    Left err -> parseError $ "Could not decode Form: " <> cs err
    Right a -> pure a


inputValue :: (InputValue a, Hyperbole :> es) => Eff es a
inputValue = do
  b <- (.body) <$> request
  case parseInputValue (cs b) of
    Left err -> parseError $ "Could not decode User Input: " <> err
    Right a -> pure a
