{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server.Response where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (Query, QueryItem)
import Web.Atomic
import Web.Hyperbole.Data.URI (URI)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.View (View)


data Response
  = Response TargetViewId (View () ())
  | NotFound
  | Redirect URI
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrQuery Text
  | ErrSession Text Text
  | ErrServer Text
  | ErrCustom Text (View () ())
  | ErrInternal
  | ErrNotHandled (Event TargetViewId Text)
  | ErrAuth Text
instance Show ResponseError where
  show = \case
    ErrParse m -> "ErrParse " <> cs m
    ErrQuery m -> "ErrQuery " <> cs m
    ErrSession k m -> "ErrSession " <> cs k <> " " <> cs m
    ErrServer m -> "ErrServer " <> cs m
    ErrCustom m _view -> "ErrCustom " <> cs m
    ErrInternal -> "ErrInternal"
    ErrNotHandled ev -> "ErrNotHandled " <> show ev
    ErrAuth m -> "ErrAuth " <> cs m
instance IsString ResponseError where
  fromString s = ErrServer (cs s)


data SerializedError = SerializedError
  { message :: Text
  , body :: BL.ByteString
  }
  deriving (Show)




serverError :: Text -> SerializedError
serverError msg =
  SerializedError msg (renderLazyByteString errorView)
 where
  errorView = el ~ pad 10 . bg (HexColor "#e53935") . color (HexColor "#fff") $ text msg
