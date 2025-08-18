module Web.Hyperbole.Types.Client where

import Data.Text (Text)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Types.Request


data Client = Client
  { requestId :: RequestId
  , session :: Cookies
  , query :: Maybe QueryData
  , pageTitle :: Maybe Text
  }


clientSetPageTitle :: Text -> Client -> Client
clientSetPageTitle t Client{session, query, requestId} =
  Client{pageTitle = Just t, session = session, query, requestId}


clientModCookies :: (Cookies -> Cookies) -> Client -> Client
clientModCookies f Client{session, query, requestId, pageTitle} =
  Client{session = f session, query, requestId, pageTitle}


clientSetQuery :: QueryData -> Client -> Client
clientSetQuery q Client{session, requestId, pageTitle} =
  Client{query = Just q, session, requestId, pageTitle}
