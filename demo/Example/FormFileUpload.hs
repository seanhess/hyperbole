{-# LANGUAGE TemplateHaskell #-}

module Example.FormFileUpload where

import App.Docs
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.Text (Text)
import Debug.Trace (traceM)
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.Request (bodyFiles, readUploadedFile)
import Web.Hyperbole.HyperView.Forms (fileInput)
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
    fs <- bodyFiles
    traceM $ show fs
    doc :: DocumentForm Identity <- formData
    cnt <- readUploadedFile doc.required.file
    pure $ submittedView doc cnt

data DocumentForm f = DocumentForm
  { name :: Field f Text
  , required :: Field f FileInfo
  , optional :: Field f (Maybe FileInfo)
  }
  deriving (Generic, FromFormF, GenFields FieldName)

-- and a view that displays an input for each field
documentFormView :: View SubmitFiles ()
documentFormView = do
  let f = fieldNames @DocumentForm
  form Submit ~ gap 15 . pad 10 . flexCol $ do
    el ~ Style.h1 $ "Upload a document"

    -- Make sure these names match the field names used by FormParse / formData
    field f.name $ do
      label $ do
        text "Your Name"
        input Username @ placeholder "Bob" ~ Style.input

    field f.required $ do
      label $ do
        text "Required File"
        fileInput

    field f.optional $ do
      label $ do
        text "Optional File"
        fileInput

    submit "Submit" ~ btn

submittedView :: DocumentForm Identity -> BL.ByteString -> View SubmitFiles ()
submittedView doc cnt = do
  el ~ bold . color Success $ "Uploaded!"

  row ~ gap 5 $ do
    el "Your Name:"
    el $ text doc.name

  el ~ underline $ "Required"
  uploadedFileView doc.required

  row ~ gap 5 $ do
    el "File Size:"
    el $ text $ cs $ show $ BL.length cnt

  el ~ underline $ "Optional"
  maybe "Not Found" uploadedFileView doc.optional

uploadedFileView :: FileInfo -> View SubmitFiles ()
uploadedFileView f = do
  row ~ gap 5 $ do
    el "File Name:"
    el $ text $ cs f.fileName

  row ~ gap 5 $ do
    el "File ContentType:"
    el $ text $ cs f.contentType

  row ~ gap 5 $ do
    el "File Source:"
    el $ text $ cs $ show f.file
