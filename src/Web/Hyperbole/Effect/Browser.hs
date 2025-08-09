module Web.Hyperbole.Effect.Browser where

import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, Query, URI)
import Web.Hyperbole.Effect.Server.Response (ResponseError (..))
import Web.Hyperbole.Effect.Server.Types (Host)


-- You can do ALL of these things from either a page or a handler

data Client = Client
  { session :: Cookies
  , query :: QueryData
  }


data PageInfo = PageInfo
  { host :: Host
  , path :: Path
  }


data Interrupt
  = NotFound
  | Redirect URI
  | Err ResponseError
  deriving (Show)


-- then we can have 2 runners. One that runs in a handler, and another that runs in Wai
data Browser :: Effect where
  -- information about the host page
  GetPageInfo :: Browser m PageInfo
  -- stops execution and returns the interrupt
  InterruptWith :: Interrupt -> Browser m a
  -- browser info
  PutClient :: Client -> Browser m ()
  GetClient :: Browser m Client


type instance DispatchOf Browser = 'Dynamic


interrupt :: (Browser :> es) => Interrupt -> Eff es a
interrupt i = send $ InterruptWith i


pageInfo :: (Browser :> es) => Eff es PageInfo
pageInfo = send GetPageInfo


modClient :: (Browser :> es) => (Client -> Client) -> Eff es ()
modClient f = do
  -- BUG: concurrency issues if multiple requests are happening at once?
  -- they still aren't allowed to do 2 updates at once... Must wait for the previous one to finish
  c <- send GetClient
  send $ PutClient $ f c


client :: (Browser :> es) => Eff es Client
client = send GetClient
