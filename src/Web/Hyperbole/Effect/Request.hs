{-# LANGUAGE RecordWildCards #-}

module Web.Hyperbole.Effect.Request where

import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.Data.URI (Path (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response


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
  let ef = urlDecodeForm b
  either (send . RespondNow . Err . ErrParse . cs) pure ef
