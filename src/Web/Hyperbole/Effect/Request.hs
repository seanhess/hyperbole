module Web.Hyperbole.Effect.Request where

import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Data.URI (Path (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Types.Request


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


bodyParams :: (Hyperbole :> es) => Eff es [Param]
bodyParams = do
  r <- request
  pure r.body.params


bodyFiles :: (Hyperbole :> es) => Eff es [File BL.ByteString]
bodyFiles = do
  r <- request
  pure r.body.files


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es Path
reqPath = (.path) <$> request
