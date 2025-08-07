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


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {text :: Text}
  deriving (Show)


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action


lookupEvent :: Query -> Maybe (Event TargetViewId Text)
lookupEvent q = do
  viewId <- TargetViewId <$> lookupParamQueryString "hyp-id" q
  action <- lookupParamQueryString "hyp-action" q
  pure $ Event{viewId, action}


-- | Lower-level lookup straight from the request
lookupParamQueryString :: ByteString -> Query -> Maybe Text
lookupParamQueryString key q = do
  mval <- L.lookup key q
  val <- mval
  pure $ cs val


isSystemParam :: QueryItem -> Bool
isSystemParam ("hyp-id", _) = True
isSystemParam ("hyp-action", _) = True
isSystemParam _ = False


serverError :: Text -> SerializedError
serverError msg =
  SerializedError msg (renderLazyByteString errorView)
 where
  errorView = el ~ pad 10 . bg (HexColor "#e53935") . color (HexColor "#fff") $ text msg
