module Web.Hyperbole.Types.Request
  ( Request (..)
  , Host (..)
  , RequestBody (..)
  , Param
  , FileParam
  , File
  , RequestId (..)
  , FileInfo (..)
  , TempFile (..)
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai.Parse (File, Param)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI (Path, Query)
import Web.Hyperbole.Types.Event (Event (..), TargetViewId)


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


newtype TempFile = TempFile {filePath :: FilePath}
  deriving (Show, Eq)


data FileInfo = FileInfo
  { file :: TempFile
  , fileName :: Text -- the original file name
  , contentType :: Text -- the mime type
  }
  deriving (Show, Eq)


type FileParam = (BS.ByteString, FileInfo)


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
