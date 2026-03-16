module Web.Hyperbole.Server.Uploads where

import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState)
import Data.String.Conversions (cs)
import Effectful
import Effectful.Exception (bracket)
import Network.Wai qualified as Wai
import Network.Wai.Parse (File, FileInfo (..), ParseRequestBodyOptions)
import Network.Wai.Parse qualified as Wai
import Web.Hyperbole.Types.Request (FileParam, Param, UploadedFile (..))


withPostBody :: (IOE :> es) => ParseRequestBodyOptions -> Wai.Request -> ([Param] -> [FileParam] -> Eff es a) -> Eff es a
withPostBody options req action = withUploadState $ \state -> do
  (params, files) <- acceptFileUploads state
  action params $ fmap fileParam files
 where
  withUploadState :: (IOE :> es) => (InternalState -> Eff es a) -> Eff es a
  withUploadState = bracket createInternalState closeInternalState

  acceptFileUploads :: (IOE :> es) => InternalState -> Eff es ([Param], [File FilePath])
  acceptFileUploads state = do
    liftIO $ Wai.parseRequestBodyEx options (Wai.tempFileBackEnd state) req

  fileParam :: Wai.File FilePath -> FileParam
  fileParam (key, f) = (key, fileInfo f)

  fileInfo :: Wai.FileInfo FilePath -> UploadedFile
  fileInfo FileInfo{fileName, fileContentType, fileContent} =
    UploadedFile
      { filePath = fileContent
      , fileName = cs fileName
      , contentType = cs fileContentType
      }
