module Web.Hyperbole.Effect.Page where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (Exception)
import Web.Atomic (el, renderLazyByteString, text)
import Web.Atomic.CSS
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.Hyperbole.Data.Cookie (CookieValue, Cookies, Key)
import Web.Hyperbole.Data.Param (Param)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, Query, URI)
import Web.Hyperbole.Types.Event (Event (..), TargetViewId (..))


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


-- You can do ALL of these things from either a page or a handler

data Client = Client
  { session :: Cookies
  , query :: QueryData
  }


-- then we can have 2 runners. One that runs in a handler, and another that runs in Wai
data Page :: Effect where
  -- information about the host page
  GetPageInfo :: Page m PageInfo
  -- stops execution and returns the interrupt
  InterruptWith :: Interrupt -> Page m a
  -- browser info
  PutClient :: Client -> Page m ()
  GetClient :: Page m Client


type instance DispatchOf Page = 'Dynamic


-- PageInfo ----------------------------------------------

pageInfo :: (Page :> es) => Eff es PageInfo
pageInfo = send GetPageInfo


data PageInfo = PageInfo
  { host :: Host
  , path :: Path
  }


-- Client -------------------------------------------------

modClient :: (Page :> es) => (Client -> Client) -> Eff es ()
modClient f = do
  -- BUG: concurrency issues if multiple requests are happening at once?
  -- they still aren't allowed to do 2 updates at once... Must wait for the previous one to finish
  c <- send GetClient
  send $ PutClient $ f c


client :: (Page :> es) => Eff es Client
client = send GetClient


-- Interrupts -----------------------------------------

interrupt :: (Page :> es) => Interrupt -> Eff es a
interrupt i = send $ InterruptWith i


data Interrupt
  = NotFound
  | Redirect URI
  | Err PageError
  deriving (Show)


pageError :: (Page :> es) => PageError -> Eff es a
pageError e = interrupt $ Err e


{- | Respond immediately with 404 Not Found

@
#EMBED Example/Docs/App.hs findUser

#EMBED Example/Docs/App.hs userPage
@
-}
notFound :: (Page :> es) => Eff es a
notFound = interrupt NotFound


-- | Redirect immediately to the 'Url'
redirect :: (Page :> es) => URI -> Eff es a
redirect = interrupt . Redirect


-- Forms could definitely be submitted over HTTP
-- AND you need to access them from hyperbole... yuck!
-- .... hmmm
-- so there ARE some things that are common between requests, etc.
-- no... we could pass Form into the action, that would make a little more sense anyway?
--
-- But then how do you access it here?
-- What would it mean to attempt to read it here?
--
-- Do you have to define an http endpoint separately?
--
-- Do we keep the original request around all the time for the user? That might be interesting...
-- Wai.Request - it's just there, along with the body
--
-- It's a security risk to always read the body though, I think...
--
-- We don't have the original wai request do we? The websocket request comes in separately...
-- we can spoof it, but we don't have a wai request...
--
-- ...
-- PageRequest
-- HasPage
-- Page
-- Page :> es
-- PageRoot
-- Page would make A LOT of sense as the name of this...
--
-- but, not if we are also trying to give it the form body...
-- well, yeah, maybe, it's the form body of the request itself...

-- well, we have errors related to the parsing of specific things... like

data PageError
  = InvalidCookies String BS.ByteString
  | InvalidForm String BL.ByteString
  | BadSession String Key CookieValue
  | BadQuery String QueryData
  | BadQueryParam String Param QueryData
  | BadFormData String Form
  | CustomError String BS.ByteString
  | InternalError String
  | NotHandled (Event TargetViewId Text)
  | BadAuth String
  deriving (Show)
instance IsString PageError where
  fromString s = InternalError (cs s)
