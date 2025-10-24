{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Chat where

import Control.Monad (forM_, forever)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.AppRoute
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber as Cyber (btn, font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar [(Username, Text)]) :> es) => Page es '[Content, Chats, Message]
page = do
  pure $ exampleLayout Chat $ do
    example Chat $ do
      el "Demonstrates server pushes"
      col ~ embed . Cyber.font $ do
        hyper Content $ contentView Nothing

type Username = Text

data Content = Content
  deriving (Generic, ViewId)

instance HyperView Content es where
  data Action Content = Login | Logout
    deriving (Generic, ViewAction)

  type Require Content = '[Chats, Message]

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
        hyper Chats $ chatsLoad u
        hyper Message $ messageView u

-- Chats State Effect -------------------------------------

-- it's just not a very good use-case for it...

initChats :: (Concurrent :> es) => Eff es (TVar [(Username, Text)])
initChats = newTVarIO []

getChats :: (Concurrent :> es, Reader (TVar [(Username, Text)]) :> es) => Eff es [(Username, Text)]
getChats = do
  var <- ask
  readTVarIO var

--- Show Chat Updates -------------------------------

data Chats = Chats
  deriving (ViewId, Generic)

instance (Concurrent :> es, Reader (TVar [(Username, Text)]) :> es, IOE :> es) => HyperView Chats es where
  data Action Chats = Stream Username
    deriving (Generic, ViewAction)

  -- it doesn't cancel the other stream if you do a new one, just keeps sending
  -- type Concurrency Chats = Replace

  update (Stream u) = do
    forever streamChats
   where
    streamChats = do
      -- this will get cancelled if the user leaves the page, and the socket connection disappears
      -- TODO: how do you go about cancelling long-running actions in haskell? Throw an async exception?
      --  you could track people's threadId and then kill them....
      --
      -- honestly this would be better with polling, and isn't that good without append/prepend...
      chats <- getChats
      liftIO $ putStrLn $ "CHATS => " <> unpack u <> " " <> show (length chats)
      pushUpdate $ chatsView u chats
      threadDelay 1000000

chatsLoad :: Username -> View Chats ()
chatsLoad user = el @ onLoad (Stream user) 100 $ "..."

chatsView :: Username -> [(Username, Text)] -> View Chats ()
chatsView _user chats = do
  col ~ gap 5 . pad 5 . minHeight 400 . border 1 . bg GrayLight $ do
    forM_ chats $ \(u, msg) -> do
      el $ do
        text u
        text ": "
        text msg

--- New Chat Messages ------------------------------

data Message = Message
  deriving (ViewId, Generic)

instance (Concurrent :> es, Reader (TVar [(Username, Text)]) :> es, IOE :> es) => HyperView Message es where
  data Action Message = NewMessage Username
    deriving (Generic, ViewAction)

  type Require Message = '[Chats]

  update (NewMessage u) = do
    MessageForm msg <- formData
    cvar <- ask
    atomically $ modifyTVar cvar $ \cs -> (u, msg) : cs

    pure $ messageView u

data MessageForm = MessageForm
  { message :: Text
  }
  deriving (Generic, FromForm)

messageView :: Username -> View Message ()
messageView u = do
  form (NewMessage u) ~ flexRow . gap 10 $ do
    field "message" $ do
      input TextInput @ placeholder "type your message here" . value "" . autofocus ~ Style.input . grow
    submit "Send" ~ btn
