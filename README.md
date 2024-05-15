Hyperbole
=========

Create dynamic HTML applications in server-side Haskell. Inspired by HTMX


Obligatory Counter Example
--------------------------


```haskell
page :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Page es Response
page var = do
  hyper $ counter var

  load $ do
    pure $ col (pad 20 . gap 10) $ do
      el h1 "Counter"
      viewId Counter (viewCount 0)


data Counter = Counter
  deriving (Show, Read, Param)
instance HyperView Counter where
  type Action Counter = Count


data Count
  = Increment
  | Decrement
  deriving (Show, Read, Param)


counter :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Counter -> Count -> Eff es (View Counter ())
counter var _ Increment = do
  n <- modify var $ \n -> n + 1
  pure $ viewCount n
counter var _ Decrement = do
  n <- modify var $ \n -> n - 1
  pure $ viewCount n


viewCount :: Int -> View Counter ()
viewCount n = col (gap 10) $ do
  row id $ do
    el (bold . fontSize 48 . border 1 . pad (XY 20 0)) $ text $ pack $ show n
  row (gap 10) $ do
    button Increment Style.btn "Increment"
    button Decrement Style.btn "Decrement"
```




Motivation
---------------------

Single Page Applications require us to write two programs: a Javascript client and a Server-side API. Hyperbole instead allows us to write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in various languages, including HTMX, Phoenix LiveView, and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe pages, views, actions and routes
3. Fast updates over sockets using virtual DOM 
4. Fall-back to HTTP

An example Page
---------------

Hyperbole applications direct URL patterns to different *Page*s. We use a `load` handler to create the entire page.

```haskell
module Page.Messages where
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es Response
page = do
  load $ do
    pure $ col (gap 10) $ do
      el h1 "Hello World"
```

Our top-level *Page* can be divided up into unique *Views*

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  load $ do
    pure $ col (gap 10) $ do
      el h1 "My Page"
      viewId (Message 1) $ messageView "Hello"
      viewId (Message 2) $ messageView "World"

data Message = Message Int
  deriving (Show, Read, Param)

messageView :: Text -> View Message ()
messageView msg = do
  el_ msg

```
Views associate interactions from UI elements with *Actions*. We specify a server-side handler for Actions which returns new HTML. Actions can perform side effects, like reading and writing data to a database.

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  hyper message
  load $ do
    ...

data Message = Message Int
  deriving (Show, Read, Param)
instance HyperView Message where
  type Action Message = MessageAction


data MessageAction = SetMessage Text
  deriving (Show, Read, Param)


message :: (Hyperbole :> es, MessageDatabase :> es) => Message -> MessageAction -> Eff es ()
message (Message n) (SetMessage msg) =
  send $ SaveMessage msg
  pure $ messageView msg


messageView :: Text -> View Message ()
messageView msg = col (gap 10) $ do
  el_ "Current Message"
  el_ msg
  button (SetMessage "A new message") id "Set Message"
```

When the user clicks the button, the server runs the `message` handler, which returns new HTML. Only the view containing the button is updated via virtual DOM, while the rest of the page remains the same


Combining multiple pages into an application
--------------------------------------------

Create a sum type to specify routes

```haskell
import Page.Messages qualified as Messages

data AppRoute
  = Main
  | Messages
  deriving (Show, Generic, Eq, Route)
```

Write a *router* function that maps routes to pages

```haskell
router :: Hyperbole :> es => AppRoute -> Eff es Response
router Messages = page Messages.page
router Main = do
  view $ do
    el_ "click a link below to visit a page"
    route Messages id "Messages"
```

Create a WAI application from your router by specifying a function to turn page fragments into full web pages on first load. Make sure to include the embedded javascript

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Network.Wai
import Data.String.Interpolate (i)
import Web.Hyperbole (..., scriptEmbed, cssResetEmbed)

app :: Application
app = do
  liveApp toDocument $ routeRequest router


toDocument :: BL.ByteString -> BL.ByteString
toDocument cnt =
  [i|<html>
    <head>
      <title>My Application</title>
      <script type="text/javascript">#{scriptEmbed}</script>
      <style type type="text/css">#{cssResetEmbed}</style>
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


Examples
---------

See the examples directory for an example application. To run them locally, clone this repository and use `cabal run`


1. [Main](./example/Main.hs) - Routing, WAI application, running the program
2. [Counter](./example/Example/Counter.hs) - Basics, State with a TVar
3. [Contacts](./example/Example/Contacts.hs) - Custom data effects, multiple views, targeting other views
4. [CSS Transitions](./example/Example/Transitions.hs)
5. [Forms](./example/Example/Forms.hs)
6. [Sessions](./example/Example/Forms.hs) - store data on the client with a session
7. [Redirects](./example/Example/Redirects.hs) - Redirecting to other pages. See Main as well
8. [Lazy Loading](./example/Example/LazyLoading.hs) - Redirecting to other pages. See Main as well
9. [Errors](./example/Example/Errors.hs) - Render server errors







