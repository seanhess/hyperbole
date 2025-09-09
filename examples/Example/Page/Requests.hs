module Example.Page.Requests where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Style.Cyber as Cyber (btn, btn', font)
import Example.View.Layout (embed, example, example', exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

page :: (Hyperbole :> es) => Page es '[CheckRequest, ControlResponse, ControlClient]
page = do
  r <- request
  pure $ exampleLayout Requests $ do
    example Requests $ do
      el "The Hyperbole Effect allows us to skip the normal update cycle to directly access the Request or manipulate the Client"
      col ~ embed . Cyber.font $ hyper CheckRequest $ viewRequest r
      col ~ embed . Cyber.font $ hyper ControlClient viewClient

    example' "Response" (routeSource Requests) $ do
      el "It also allows us to directly affect the response and the javascript client"
      col ~ embed . Cyber.font $ hyper ControlResponse responseView

-- REQUEst -------------------------------------------------

data CheckRequest = CheckRequest
  deriving (Generic, ViewId)

instance HyperView CheckRequest es where
  data Action CheckRequest
    = Refresh
    deriving (Generic, ViewAction)

  update Refresh = do
    r <- request
    pure $ viewRequest r

viewRequest :: Request -> View CheckRequest ()
viewRequest r = do
  col ~ gap 10 $ do
    el $ do
      text "Host: "
      text $ cs $ show r.host
    el $ do
      text "Path: "
      text $ cs $ show r.path
    el $ do
      text "Query: "
      text $ cs $ show r.query
    el $ do
      text "Cookies: "
      text $ cs $ show r.cookies

-- CLIENT -------------------------------------------------

data Message = Message
  { message :: Text
  }
  deriving (Generic, ToQuery)

data ControlClient = ControlClient
  deriving (Generic, ViewId)

instance HyperView ControlClient es where
  type Require ControlClient = '[CheckRequest]

  data Action ControlClient
    = SetQuery
    deriving (Generic, ViewAction)

  update SetQuery = do
    setQuery $ Message "hello"
    trigger CheckRequest Refresh
    pure "Updated Query String"

viewClient :: View ControlClient ()
viewClient = do
  button SetQuery ~ btn $ "Set Query String from another HyperView"

-- RESPONSE -------------------------------------------------

data ControlResponse = ControlResponse
  deriving (Generic, ViewId)

instance HyperView ControlResponse es where
  data Action ControlResponse
    = RedirectAsAction
    | SetPageTitle
    | RespondNotFound
    | -- \| RespondEarlyView
      RespondWithError
    deriving (Generic, ViewAction)
  update RedirectAsAction = do
    redirect $ pathUri "/hello/redirected"
  update SetPageTitle = do
    pageTitle "Hello World!"
    pure $ col ~ gap 10 $ do
      el ~ bold $ "Set page title!"
      responseView
  update RespondNotFound = do
    _ <- notFound
    pure "This will not be rendered"
  -- update RespondEarlyView = do
  --   _ <- respondView ControlResponse "Responded early!"
  --   pure "This will not be rendered"
  update RespondWithError = do
    _ <- respondError "Some custom error"
    pure "This will not be rendered"

responseView :: View ControlResponse ()
responseView = do
  row ~ gap 10 . flexWrap Wrap $ do
    button RedirectAsAction ~ btn $ "Redirect Me"
    button SetPageTitle ~ btn $ "Set Page Title"
    button RespondNotFound ~ btn' Danger $ "Respond Not Found"
    button RespondWithError ~ btn' Danger $ "Respond Error"
