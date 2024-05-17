Hyperbole
=========

Create interactive HTML applications with type-safe serverside Haskell. Inspired by HTMX.

Depends on:
* [Effectful](https://hackage.haskell.org/package/effectful)
* [Web View](https://hackage.haskell.org/package/web-view)


Simple Example
--------------

```haskell
import Data.Text (Text)
import Web.Hyperbole


main :: IO ()
main = do
  -- Warp.run on port 3000
  run 3000 $ do
    -- create a WAI.Application
    liveApp (basicDocument "Example") $ do
      -- A single page
      page $ do
        -- handle message actions
        hyper message
        -- handle initial page load
        load $ do
          -- after side effects, render entire page once
          pure viewPage


-- render entire page
viewPage :: View c ()
viewPage = do
  -- this part never changes
  el bold "My Page"
  -- register a view with Id = Message, which updates itself with vdom
  viewId Message $ messageView "HELLO WORLD"


-- Unique View Id
data Message = Message
  deriving (Generic, Param)


-- Actions for Message Views
data MessageAction = SetMessage Text
  deriving (Generic, Param)


instance HyperView Message where
  type Action Message = MessageAction


-- Handle Message actions
message :: Message -> MessageAction -> Eff es (View Message ())
message _ (SetMessage m) = do
  -- After side effects, re-render the view with new data
  pure $ messageView m


-- Render a message view
messageView :: Text -> View Message ()
messageView m = col id $ do
  el_ "Message:"
  el_ $ text m
  button (SetMessage "Goodbye") id "Goodbye"
```



Why Hyperbole?
--------------

Single Page Applications require us to write two programs: a Javascript client and a Server-side API. Hyperbole allows us instead to write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in various languages, including HTMX, Phoenix LiveView, and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe views, actions, routes, and forms
3. Elegant interface with little boilerplate
5. Fast updates over sockets using virtual DOM 
6. Fall-back to HTTP



Creating an Application, Step-by-Step
-------------------------------------

Hyperbole applications direct URL patterns to different *Page*s. We use a `load` handler for when the user first visits the page.

```haskell
module Page.Messages where
import Web.Hyperbole

-- static page
page :: (Hyperbole :> es) => Page es Response
page = do
  -- this runs when the user visits the page's url
  load $ do
    -- any load side effects go here
    -- render this HTML
    pure $ col (gap 10) $ do
      el h1 "Hello World"
```

Our top-level *Page* can be divided up into unique interactive *Views*

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  load $ pure $ do
    -- the outer HTML never changes
    col (gap 10) $ do
      el h1 "My Page"
      -- Each view updates independently
      viewId (Message 1) $ messageView "Hello"
      viewId (Message 2) $ messageView "World"

-- Views need an id unique to the page
data Message = Message Int
  deriving (Generic, Param)

-- Function that renders Message Views
messageView :: Text -> View Message ()
messageView msg = do
  el_ msg

```
Views turn UI interactions into server *Actions*. We specify handlers for Actions which return new HTML. Actions can perform side effects, like reading and writing data to a database.

```haskell
page :: (Hyperbole :> es) => Page es Response
page = do
  -- Add a handler for message actions
  hyper message
  load $ do
    ...

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

The [example](./example/) directory contains an example application with multiple pages demonstrating different features. To run them locally, clone this repository and use `cabal run`


### [Main](./example/Main.hs)

Routing, WAI application, running the program

### [Counter](./example/Example/Counter.hs)

Basics, State with a TVar

<img src="/example/doc/counter.gif" height="225"/>

### [Contacts](./example/Example/Contacts.hs)

Custom data effects, multiple views, targeting other views, loading indicators

<img src="/example/doc/contacts.gif" height="200"/>


### [CSS Transitions](./example/Example/Transitions.hs)

Animate transitions using only CSS

<img src="/example/doc/transitions.gif" height="120"/>

### [Forms](./example/Example/Forms.hs)

Elegant interface for Forms including field-specific validation 

<img src="/example/doc/forms.gif" height="450"/>

### [Sessions](./example/Example/Forms.hs)
store data on the client with a session

<img src="/example/doc/sessions.gif" height="200"/>

### [Redirects](./example/Example/Redirects.hs)
Redirecting to other pages. See Main as well

<img src="/example/doc/redirects.gif" height="250"/>

### [Lazy Loading and Polling](./example/Example/LazyLoading.hs)
Run an action after a delay for lazy loading or polling. See Main as well

<img src="/example/doc/lazy.gif" height="250"/>

### [Errors](./example/Example/Errors.hs)
Render server errors

<img src="/example/doc/errors.gif" width="300"/>







