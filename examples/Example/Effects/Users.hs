{-# LANGUAGE LambdaCase #-}

module Example.Effects.Users where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Example.AppRoute (UserId)
import Web.Hyperbole (Hyperbole, notFound)

data User = User
  { id :: UserId
  , firstName :: Text
  , lastName :: Text
  , age :: Int
  , info :: Text
  , isActive :: Bool
  }
  deriving (Show)

-- Load a user AND do next if missing?
data Users :: Effect where
  LoadUser :: UserId -> Users m (Maybe User)
  LoadUsers :: Users m [User]
  SaveUser :: User -> Users m ()
  ModifyUser :: UserId -> (User -> User) -> Users m ()
  DeleteUser :: UserId -> Users m ()
  NextId :: Users m UserId

type instance DispatchOf Users = 'Dynamic

type UserStore = MVar (Map UserId User)

runUsersIO
  :: (IOE :> es)
  => UserStore
  -> Eff (Users : es) a
  -> Eff es a
runUsersIO var = interpret $ \_ -> \case
  LoadUser uid -> do
    us <- liftIO $ readMVar var
    pure $ M.lookup uid us
  LoadUsers -> loadAll
  SaveUser u -> do
    modify $ \us -> pure $ M.insert u.id u us
  ModifyUser uid f -> do
    modify $ \us -> do
      pure $ M.adjust f uid us
  DeleteUser uid -> do
    modify $ \us -> pure $ M.delete uid us
  NextId -> do
    us <- loadAll
    let umax = maximum $ fmap (.id) us
    pure (umax + 1)
 where
  loadAll :: (MonadIO m) => m [User]
  loadAll = do
    us <- liftIO $ readMVar var
    pure $ M.elems us

  modify :: (MonadIO m) => (Map UserId User -> IO (Map UserId User)) -> m ()
  modify f = liftIO $ modifyMVar_ var f

initUsers :: (MonadIO m) => m UserStore
initUsers =
  liftIO $ newMVar $ M.fromList $ map (\u -> (u.id, u)) users
 where
  users =
    [ User 1 "Joe" "Blow" 32 "" True
    , User 2 "Sara" "Dane" 24 "" False
    , User 3 "Billy" "Bob" 48 "" False
    , User 4 "Felicia" "Korvus" 84 "" True
    ]

find :: (Hyperbole :> es, Users :> es) => Int -> Eff es User
find uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu

all :: (Users :> es) => Eff es [User]
all = send LoadUsers

save :: (Users :> es) => User -> Eff es ()
save = send . SaveUser

delete :: (Users :> es) => Int -> Eff es ()
delete = send . DeleteUser

nextId :: (Users :> es) => Eff es Int
nextId = send NextId
