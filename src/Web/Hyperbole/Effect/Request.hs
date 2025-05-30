{-# LANGUAGE RecordWildCards #-}

module Web.Hyperbole.Effect.Request where

import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.Data.URI (Path)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = reqRemoveSystem <$> send GetRequest


reqRemoveSystem :: Request -> Request
reqRemoveSystem Request{..} =
  Request{query = filter (not . isSystemParam) query, ..}


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
  either (send . RespondEarly . Err . ErrParse) pure ef
