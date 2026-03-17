{-# LANGUAGE TemplateHaskell #-}

module Example.FormFileUpload where

import App.Docs
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.Text (Text)
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.Request (readUploadedFile)
import Web.Hyperbole.Types.Request (FileInfo (..))

-- TODO: should we error out on parse if they expect a file, but it is empty? Probably!

source :: ModuleSource
source = $(moduleSource)

data SubmitFiles = SubmitFiles
  deriving (Generic, ViewId)

instance HyperView SubmitFiles es where
  data Action SubmitFiles
    = Submit
    deriving (Generic, ViewAction)

  update Submit = do
    doc :: DocumentForm <- formData
    cnt <- readUploadedFile doc.uploaded.file
    pure $ uploadedFileView doc cnt

data DocumentForm = DocumentForm
  { name :: Text
  , uploaded :: FileInfo
  }
  deriving (Generic, FromForm)

-- and a view that displays an input for each field
documentFormView :: View SubmitFiles ()
documentFormView = do
  form Submit ~ gap 15 . pad 10 . flexCol $ do
    el ~ Style.h1 $ "Upload a document"

    -- Make sure these names match the field names used by FormParse / formData
    field "name" $ do
      label $ do
        text "Your Name"
        input Username @ placeholder "Bob" ~ Style.input

    field "file" $ do
      label $ do
        tag "input" @ att "type" "file" . att "name" "file" $ none

    submit "Submit" ~ btn

uploadedFileView :: DocumentForm -> BL.ByteString -> View SubmitFiles ()
uploadedFileView doc cnt = do
  el ~ bold . color Success $ "Uploaded!"

  row ~ gap 5 $ do
    el "Your Name:"
    el $ text doc.name

  let f = doc.uploaded
  row ~ gap 5 $ do
    el "File Name:"
    el $ text $ cs f.fileName

  row ~ gap 5 $ do
    el "File ContentType:"
    el $ text $ cs f.contentType

  row ~ gap 5 $ do
    el "File Source:"
    el $ text $ cs $ show f.file

  row ~ gap 5 $ do
    el "File Size:"
    el $ text $ cs $ show $ BL.length cnt
