module Web.Hyperbole.Effect.Server.Types where

import Control.Exception (Exception)
import Data.Aeson (ToJSON)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (Method, Query)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path)
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Types.Event (RequestId (..))


-- -- | Low level effect mapping request/response to either HTTP or WebSockets
-- data Server :: Effect where
--   LoadRequest :: Server m Request
--   SendResponse :: Client' -> Response -> Server m ()
--
--
-- type instance DispatchOf Server = 'Dynamic
--
--
-- data Client' = Client'



-- newtype Metadata = Metadata [(BL.ByteString, Text)] deriving newtype (Semigroup, Monoid)


-- data Request = Request
--   { host :: Host
--   , path :: Path
--   , query :: Query
--   , body :: BL.ByteString
--   , method :: Method
--   , cookies :: Cookies
--   , requestId :: RequestId
--   }
--   deriving (Show)
