module Example.Page.Requests where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Style as Style
import Example.View.Layout (embed, example, exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

page :: (Hyperbole :> es) => Eff es (Page '[CheckRequest, ControlResponse, ControlClient])
page = do
  r <- request
  pure $ exampleLayout Route.Requests $ do
    example "Requests" source $ do
      el "The Hyperbole Effect allows us to access the Request, and manipulate the Client"
      col ~ embed $ hyper CheckRequest $ viewRequest r
      col ~ embed $ hyper ControlClient viewClient

    example "Response" source $ do
      el "It also allows us to directly affect the response"
      col ~ embed $ hyper ControlResponse responseView
 where
  source = "Example/Page/Requests.hs"

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

    button Refresh ~ Style.btnLight $ "Refresh"

-- CLIENT -------------------------------------------------

data Message = Message
  { message :: Text
  }
  deriving (Generic, ToQuery)

data ControlClient = ControlClient
  deriving (Generic, ViewId)

instance HyperView ControlClient es where
  data Action ControlClient
    = SetQuery
    deriving (Generic, ViewAction)

  update SetQuery = do
    setQuery $ Message "hello"
    pure "Updated Query String"

viewClient :: View ControlClient ()
viewClient = do
  button SetQuery ~ Style.btn $ "Set Query String"

-- RESPONSE -------------------------------------------------

data ControlResponse = ControlResponse
  deriving (Generic, ViewId)

instance HyperView ControlResponse es where
  data Action ControlResponse
    = RedirectAsAction
    | RespondNotFound
    | RespondEarlyView
    | RespondWithError
    deriving (Generic, ViewAction)
  update RedirectAsAction = do
    redirect $ pathUri "/hello/redirected"
  update RespondNotFound = do
    _ <- notFound
    pure "This will not be rendered"
  update RespondEarlyView = do
    _ <- respondView ControlResponse "Responded early!"
    pure "This will not be rendered"
  update RespondWithError = do
    _ <- respondError "Some custom error"
    pure "This will not be rendered"

responseView :: View ControlResponse ()
responseView = do
  row ~ gap 10 $ do
    button RedirectAsAction ~ Style.btn $ "Redirect Me"
    button RespondEarlyView ~ Style.btn $ "Respond Early"
    button RespondNotFound ~ Style.btn' Danger $ "Respond Not Found"
    button RespondWithError ~ Style.btn' Danger $ "Respond Error"
