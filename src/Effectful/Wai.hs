{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

module Effectful.Wai where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (Status, status200, status500)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai as Wai
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData)
import Web.View


data Wai :: Effect where
  ResHeader :: HeaderName -> ByteString -> Wai m ()
  ResBody :: ContentType -> L.ByteString -> Wai m ()
  ResStatus :: Status -> Wai m ()
  ReqBody :: Wai m L.ByteString
  Request :: Wai m Request
  Continue :: Wai m ()
  Interrupt :: Interrupt -> Wai m a


type instance DispatchOf Wai = 'Dynamic


data Handler = Handler
  { request :: Request
  , cachedRequestBody :: L.ByteString
  , status :: Status
  , headers :: [(HeaderName, ByteString)]
  , contentType :: ContentType
  , body :: L.ByteString
  }


data ContentType
  = ContentHtml
  | ContentText


formData :: (Wai :> es) => Eff es Form
formData = do
  bd <- send ReqBody
  let ef = urlDecodeForm bd
  either (send . Interrupt . ParseError) pure ef


parseFormData :: (Wai :> es, FromForm a) => Eff es a
parseFormData = do
  f <- formData
  either (send . Interrupt . ParseError) pure $ fromForm f


formParam :: (Wai :> es, FromHttpApiData a) => Text -> Eff es a
formParam k = do
  f <- formData
  either (send . Interrupt . ParseError) pure $ parseUnique k f


requestBody :: (Wai :> es) => Eff es L.ByteString
requestBody = send ReqBody


runWai
  :: (IOE :> es)
  => Request
  -> Eff (Wai : es) a
  -> Eff es (Either Interrupt Handler)
runWai req = reinterpret runLocal $ \_ -> \case
  Request -> do
    gets request
  ReqBody -> do
    cacheReqBody
    gets cachedRequestBody
  ResHeader k v -> modify
    $ \r -> r{headers = (k, v) : r.headers}
  ResStatus s -> modify
    $ \r -> r{status = s}
  ResBody ct bd -> modify
    $ \r -> r{body = bd, contentType = ct, status = status200}
  Continue -> do
    h <- get
    throwError $ RespondNow h
  Interrupt e -> do
    throwError e
 where
  runLocal =
    runErrorNoCallStack @Interrupt
      . execState @Handler (emptyResponse req)

  cacheReqBody :: forall es. (IOE :> es, State Handler :> es) => Eff es ()
  cacheReqBody = do
    r <- get
    when (L.null r.cachedRequestBody) $ do
      rb <- liftIO $ Wai.consumeRequestBodyLazy req
      put $ r{cachedRequestBody = rb}


-- Ends computation with whatever has already been set on the response
continue :: (Wai :> es) => Eff es ()
continue = send Continue


emptyResponse :: Request -> Handler
emptyResponse r = Handler r "" status500 [] ContentText "Response not set"


data Interrupt
  = NotFound
  | Redirect Url
  | ParseError Text
  | RespondNow Handler


notFound :: (Wai :> es) => Eff es a
notFound = send $ Interrupt NotFound


redirect :: (Wai :> es) => Url -> Eff es ()
redirect u = do
  send $ Interrupt $ Redirect u
