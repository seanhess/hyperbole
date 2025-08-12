{-# LANGUAGE RecordWildCards #-}

module Web.Hyperbole.Effect.Request where

import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.Data.URI (Path (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Page
import Web.Hyperbole.Effect.Server


-- -- | Return all information about the 'Request'
-- request :: (Hyperbole :> es) => Eff es Request
-- request = reqRemoveSystem <$> send GetRequest

-- reqRemoveSystem :: Request -> Request
-- reqRemoveSystem Request{..} =
--   Request{query = filter (not . isSystemParam) query, ..}

{- | Return the request path

>>> reqPath
["users", "100"]
-}

-- reqPath :: (Hyperbole :> es) => Eff es Path
-- reqPath = (.paoth) <$> request

{- | Return the request body as a Web.FormUrlEncoded.Form

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Page :> es) => Eff es Form
formBody = do
  b <- _ -- (.body) <$> request
  let ef = urlDecodeForm b
  either (\e -> pageError $ InvalidForm (cs e) b) pure ef
