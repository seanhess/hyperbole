
Creating an Application, Step-by-Step
-------------------------------------

Hyperbole applications direct URL patterns to different *Page*s. We use a `load` handler for when the user first visits the page.

```haskell
module Page.Messages where
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es Response
page = do
  -- this runs when the user visits the page's url
  load $ do
    -- any load side effects go here
    -- render the page (currently static)
    pure $ col (gap 10) $ do
      el h1 "Hello World"
```

Our top-level *Page* can be divided up into unique interactive *Views*

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  load $ pure pageView

pageView :: View c ()
pageView = do
  -- the outer HTML never changes
  col (gap 10) $ do
    el h1 "My Page"
    -- Each view updates independently
    hyper (Message 1) $ messageView "Hello"
    hyper (Message 2) $ messageView "World"

-- Views need an id unique to the page
data Message = Message Int
  deriving (Generic, Param)

-- Function that renders Message Views
messageView :: Text -> View Message ()
messageView msg = do
  el_ msg

```
Views turn UI interactions into server *Actions*. We use the 'hyper' function to specify handlers for Actions which return new HTML.

Actions can perform side effects, like reading and writing data to a database.

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  -- Add a handler for message actions
  handler message
  load $ pure pageView

data Message = Message Int
  deriving (Generic, Param)

-- Sum-type with all possible actions for Message Views
data MessageAction = SetMessage Text
  deriving (Generic, Param)


-- Handle message actions
message :: (Hyperbole :> es, MessageDatabase :> es) => Message -> MessageAction -> Eff es ()
message (Message n) (SetMessage msg) =
  -- Pretend we have a custom MessageDatabase effect
  -- perform any side effects here
  send $ SaveMessage msg
  -- now use the same view function to update the view
  pure $ messageView msg


messageView :: Text -> View Message ()
messageView msg = col (gap 10) $ do
  el_ "Current Message"
  el_ msg
  -- run action `SetMessage "A new message"` when clicked
  button (SetMessage "A new message") id "Set Message"
```

When the user clicks the button, the `SetMessage` action is sent to the server. Then `message` handles it, runs side effects, and returns new HTML.

Only the view containing the button is updated via virtual DOM, while the rest of the page remains the same


Combining multiple pages into an application
--------------------------------------------

Now that we have a page, let's create an application. First, create a sum type to specify routes

```haskell
import Page.Messages qualified as Messages

data AppRoute
  -- / or /main
  = Main
  -- /messages
  | Messages
  deriving (Eq, Generic, Route)
```

Write a *router* function that maps routes to pages

```haskell
router :: Hyperbole :> es => AppRoute -> Eff es Response
router Messages = page Messages.page
router Main = do
  -- render a static page
  view $ do
    el_ "click a link below to visit a page"
    -- type-safe hyperlink
    route Messages id "Messages"
```

Next create a WAI application from your router by specifying a function to turn page fragments into full web pages on first load. Make sure to include Hyperbole's embedded javascript

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Network.Wai
import Data.String.Interpolate (i)
import Data.ByteString.Lazy as BL
import Web.Hyperbole (..., liveApp, routeRequest, scriptEmbed, cssResetEmbed)


app :: Application
app = do
  liveApp toDocument $ routeRequest router


toDocument :: BL.ByteString -> BL.ByteString
toDocument cnt =
  [i|<html>
    <head>
      <title>My Application</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type="text/css">#{cssResetEmbed}</style>
    </head>
    <body>#{cnt}</body>
  </html>|]
```

Finally, run your application with Warp

```haskell
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = Warp.run 3000 app
```
