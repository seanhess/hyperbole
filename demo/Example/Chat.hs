{-# LANGUAGE UndecidableInstances #-}

module Example.Chat where

import App.Route
import Control.Monad (forM_, forever)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic (get, modify)
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber (embed)
import Example.Style.Cyber as Cyber (btn, font)
import Example.View.Layout (layout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.Encoded (Encoded (..), FromEncoded (..), ToEncoded (..))

page :: (Hyperbole :> es, Concurrent :> es, Reader Room :> es) => Page es '[Content, Chats, NewMessage]
page = do
  pure $ layout (Examples Chat) $ do
    el "Demonstrates server pushes and concurrency. Open in two tabs with different usernames to test."
    col ~ embed . Cyber.font $ do
      hyper Content $ contentView Nothing

type Username = Text

data Content = Content
  deriving (Generic, ViewId)

instance HyperView Content es where
  data Action Content = Login | Logout
    deriving (Generic, ViewAction)

  type Require Content = '[Chats, NewMessage]

  update Login = do
    LoginForm u <- formData
    pure $ contentView (Just u)
  update Logout =
    pure $ contentView Nothing

data LoginForm = LoginForm
  { username :: Text
  }
  deriving (Generic, FromForm)

contentView :: Maybe Username -> View Content ()
contentView mu = do
  case mu of
    Nothing -> do
      form Login ~ flexRow . gap 10 $ do
        field "username" $ do
          input Username @ placeholder "Username" . autofocus ~ Style.input
        submit "Login" ~ btn
    Just u -> do
      col ~ gap 10 $ do
        row ~ gap 10 $ do
          el "Welcome "
          el ~ bold $ text u
          space
          button Logout ~ btn $ "logout"
        hyperState Chats mempty $ chatsLoad u
        hyperState NewMessage u messageView

-- Chat Room -------------------------------------

data Message = Message
  { sender :: Username
  , body :: Text
  }
  deriving (Generic, ToParam, FromParam)

newtype Room = Room (TChan Message)
newtype Subscription = Subscription (TChan Message)

initChatRoom :: (Concurrent :> es) => Eff es Room
initChatRoom = Room <$> newBroadcastTChanIO

subscribeChatRoom :: (Concurrent :> es) => Room -> Eff es Subscription
subscribeChatRoom (Room chan) = fmap Subscription <$> atomically $ dupTChan chan

waitMessage :: (Concurrent :> es) => Subscription -> Eff es Message
waitMessage (Subscription chan) = atomically $ readTChan chan

sendMessage :: (Concurrent :> es) => Room -> Message -> Eff es ()
sendMessage (Room chan) msg = atomically $ writeTChan chan msg

-- Encoding for message history since starting
newtype AllMessages = AllMessages [Message]
  deriving newtype (Semigroup, Monoid)

instance ToEncoded AllMessages where
  toEncoded (AllMessages ms) = Encoded "" (fmap toParam ms)
instance FromEncoded AllMessages where
  parseEncoded (Encoded _ ps) =
    AllMessages <$> mapM parseParam ps

--- Chat Updates ---------------------------------------------

data Chats = Chats
  deriving (Generic)
instance ViewId Chats where
  type ViewState Chats = AllMessages

instance (Concurrent :> es, Reader Room :> es, IOE :> es) => HyperView Chats es where
  data Action Chats = Stream Username
    deriving (Generic, ViewAction)

  -- we need to build up our own list of messages...
  update (Stream u) = do
    room <- ask
    sub <- subscribeChatRoom room

    sendMessage room $ Message u "I have arrived!"

    forever (streamChats sub)
   where
    streamChats room = do
      -- this will get cancelled when the user leaves the page, on calling pushUpdate
      msg <- waitMessage room
      modify $ addMessage msg
      pushUpdate $ chatsView u

allMessages :: View Chats AllMessages
allMessages = do
  AllMessages ms <- viewState
  pure $ AllMessages $ reverse ms

addMessage :: Message -> AllMessages -> AllMessages
addMessage msg (AllMessages ms) = AllMessages $ msg : ms

-- TODO: initial message or view that shows better, since we aren't loading history any more
chatsLoad :: Username -> View Chats ()
chatsLoad user = el @ onLoad (Stream user) 100 $ "..."

chatsView :: Username -> View Chats ()
chatsView _user = do
  AllMessages chats <- allMessages
  col ~ gap 5 . pad 5 . minHeight 400 . border 1 . bg GrayLight $ do
    forM_ chats $ \chat -> do
      el $ do
        text chat.sender
        text ": "
        text chat.body

--- New Chat Messages ------------------------------

data NewMessage = NewMessage
  deriving (Generic)
instance ViewId NewMessage where
  type ViewState NewMessage = Username

instance (Concurrent :> es, Reader Room :> es, IOE :> es) => HyperView NewMessage es where
  data Action NewMessage = SendMessage
    deriving (Generic, ViewAction)

  -- type Require NewMessage = '[Chats]

  update SendMessage = do
    user <- get @Username
    room <- ask
    MessageForm msg <- formData
    sendMessage room $ Message user msg
    -- NOTE: this doesn't show an update at all, but we are subscribed to the channel and will get a push like everyone else
    pure messageView

data MessageForm = MessageForm
  { message :: Text
  }
  deriving (Generic, FromForm)

messageView :: View NewMessage ()
messageView = do
  form SendMessage ~ flexRow . gap 10 $ do
    field "message" $ do
      input TextInput @ placeholder "type your message here" . value "" . autofocus ~ Style.input . grow
    submit "Send" ~ btn
