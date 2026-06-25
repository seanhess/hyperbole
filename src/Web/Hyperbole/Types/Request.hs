module Web.Hyperbole.Types.Request
  ( Request (..)
  , Host (..)
  , Form (..)
  , FormParam (..)
  , Param
  , FileParam
  , File
  , RequestId (..)
  , UploadedFile (..)
  ) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai.Parse (File, Param)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI (Path, Query)
import Web.Hyperbole.Types.Event (Event (..), TargetViewId)


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data UploadedFile = UploadedFile
  { filePath :: FilePath
  , fileName :: Text -- the original file name
  , contentType :: Text -- the mime type
  }
  deriving (Show, Eq)


type FileParam = (BS.ByteString, UploadedFile)


data Request = Request
  { host :: Host
  , path :: Path
  , query :: Query
  , method :: Method
  , cookies :: Cookies
  , event :: Maybe (Event TargetViewId Encoded Value)
  , requestId :: RequestId
  , form :: Form
  , input :: Text
  }
  deriving (Show)


data Form = Form
  { params :: [Param]
  , files :: [FileParam]
  }
  deriving (Show)


data FormParam
  = FormParam Text
  | FileParam UploadedFile
  deriving (Show, Eq)
instance IsString FormParam where
  fromString s = FormParam (cs s)


newtype RequestId = RequestId Text
  deriving (Show, Eq)

