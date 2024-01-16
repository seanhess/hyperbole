{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Socket where

import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Network.WebSockets (Connection, ConnectionOptions, ServerApp, WebSocketsData)
import Network.WebSockets qualified as WS


data Socket :: Effect where
  SendMessage :: BL.ByteString -> Socket m ()
  ReceiveData :: (WebSocketsData a) => Socket m a


type instance DispatchOf Socket = 'Dynamic


data Client = Client
  { count :: Int
  , -- need to track which page we are talking about here.. parse!
    contents :: [String]
  }


type instance DispatchOf Socket = 'Dynamic


connectionOptions :: ConnectionOptions
connectionOptions = WS.defaultConnectionOptions


runSocketApp :: Eff [Socket, IOE] () -> ServerApp
runSocketApp talk pending = do
  conn <- WS.acceptRequest pending
  -- WS.sendTextData conn ("HELLO CLIENT" :: Text)
  let client = Client 0 []
  runEff . runSocket conn client $ forever talk


runSocket
  :: (IOE :> es)
  => Connection
  -> Client
  -> Eff (Socket : es) a
  -> Eff es a
runSocket conn client = reinterpret (evalState client) $ \_ -> \case
  SendMessage t -> do
    -- cl :: Client <- get
    liftIO $ WS.sendTextData conn t
  ReceiveData -> do
    a <- liftIO $ WS.receiveData conn
    modify $ \c -> c{count = c.count + 1}
    pure a


-- sendCommand :: (Socket :> es) => Command -> Eff es ()
-- sendCommand (Render cnt) = send $ SendMessage (formatMessage "Render" cnt)
-- sendCommand (Test cnt) = send $ SendMessage (formatMessage "Test" cnt)

receiveData :: (Socket :> es, WebSocketsData a) => Eff es a
receiveData = send ReceiveData


sendMessage :: (Socket :> es) => BL.ByteString -> Eff es ()
sendMessage msg = send $ SendMessage msg

-- formatMessage :: (ToJSON a) => BL.ByteString -> a -> BL.ByteString
-- formatMessage flag cnt =
--   let content = A.encode cnt
--    in [i|#{flag} #{content}|]
