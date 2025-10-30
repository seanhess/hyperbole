{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Server.Options where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Web.Atomic.CSS
import Web.Hyperbole.Data.Encoded (Encoded, encodedToText)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View


data ServerOptions = ServerOptions
  { toDocument :: BL.ByteString -> BL.ByteString
  , serverError :: ResponseError -> ServerError
  }


defaultErrorMessage :: ResponseError -> Text
defaultErrorMessage = \case
  -- mask server errors
  ErrCustom e -> e.message
  NotFound -> "Not Found"
  ErrInternal -> "Internal Server Error"
  ErrServer m -> m
  e -> cs $ drop 3 $ show e


defaultErrorBody :: Text -> View Body ()
defaultErrorBody msg =
  el ~ bg (HexColor "#F00") . color (HexColor "#FFF") $ do
    text msg


defaultError :: ResponseError -> ServerError
defaultError = \case
  ErrCustom e -> e
  ErrNotHandled e -> errNotHandled e
  err ->
    let msg = defaultErrorMessage err
     in ServerError msg (defaultErrorBody msg)
 where
  errNotHandled :: Event TargetViewId Encoded -> ServerError
  errNotHandled ev =
    ServerError "Action Not Handled" $ do
      el $ do
        text "No Handler for Event viewId: "
        text $ encodedToText ev.viewId.encoded
        text " action: "
        text $ encodedToText ev.action
      el $ do
        text "Remember to add a `hyper` handler in your page function"
      pre $
        T.intercalate
          "\n"
          [ "page :: (Hyperbole :> es) => Page es Response"
          , "page = do"
          , "  handle contentsHandler"
          , "  load $ do"
          , "    pure $ hyper Contents contentsView"
          , "</pre>"
          ]
