{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Types.Response where

import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Web.Hyperbole.Data.Encoded (Encoded)
import Web.Hyperbole.Data.URI (URI)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.View


data Body = Body


data Response
  = Response TargetViewId (View Body ())
  | Redirect URI
  | Err ResponseError


data ResponseError
  = NotFound
  | ErrParse String
  | ErrQuery String
  | ErrSession Text String
  | ErrServer Text
  | ErrCustom ServerError
  | ErrInternal
  | ErrNotHandled (Event TargetViewId Encoded)
  | ErrAuth Text
instance Show ResponseError where
  show = \case
    NotFound -> "NotFound"
    ErrParse m -> "ErrParse " <> cs m
    ErrQuery m -> "ErrQuery " <> cs m
    ErrSession k m -> "ErrSession " <> cs k <> " " <> cs m
    ErrServer m -> "ErrServer " <> cs m
    ErrCustom err -> "ErrCustom " <> cs err.message
    ErrInternal -> "ErrInternal"
    ErrNotHandled ev -> "ErrNotHandled " <> show ev
    ErrAuth m -> "ErrAuth " <> cs m
instance IsString ResponseError where
  fromString s = ErrServer (cs s)


-- Serialized server error
data ServerError = ServerError
  { message :: Text
  , body :: View Body ()
  }
