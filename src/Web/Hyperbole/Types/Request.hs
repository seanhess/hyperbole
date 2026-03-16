module Web.Hyperbole.Types.Request
  ( Request (..)
  , Host (..)
  , RequestBody (..)
  , Param
  , File
  , RequestId (..)
  , FileInfo (..)
  , UploadedFile
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai.Parse (File, FileInfo (..), Param)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI (Path, Query)
import Web.Hyperbole.Types.Event (Event (..), TargetViewId)


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


type UploadedFile = FileInfo BL.ByteString
type FileParam = (BS.ByteString, UploadedFile)


-- type Param = (ByteString, ByteString)
-- type File () = (ByteString, FileInfo ())
--  fileName :: ByteString
--  fileContentType :: ByteString
--  fileContent :: c
data Request = Request
  { host :: Host
  , path :: Path
  , query :: Query
  , method :: Method
  , cookies :: Cookies
  , event :: Maybe (Event TargetViewId Encoded Encoded)
  , requestId :: RequestId
  , body :: RequestBody
  }
  deriving (Show)


data RequestBody = RequestBody
  { params :: [Param]
  , files :: [FileParam]
  }
  deriving (Show)


newtype RequestId = RequestId Text
  deriving (Show)

-- -- | Parse a MessageBody as [Param]
-- parseUrlEncodedParams :: MessageBody -> Either String [Param]
-- parseUrlEncodedParams msg = do
--   let ef = urlDecodeParams msg.value
--   either (Left . cs) formToParams ef
--  where
--   formToParams :: Form -> Either String [Param]
--   formToParams = _
--
-- fromForm :: RequestBody -> Form
-- fromForm = _
