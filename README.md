Hyperbole
=========

Create interactive HTML applications with type-safe serverside Haskell. Inspired by HTMX.

Depends on:
* [Effectful](https://hackage.haskell.org/package/effectful)
* [Web View](https://hackage.haskell.org/package/web-view)


Simple Example
--------------------------

```haskell
import Data.Text (Text)
import Web.Hyperbole


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
        load $ do
          -- TODO - Perform load side-effects
          pure viewPage


-- render entire page once
viewPage :: View c ()
viewPage = do
  el bold "My Page"
  -- register a view with Id = Msg
  -- it will update itself
  viewId Msg $ viewMsg "HELLO WORLD"


-- Unique View Id
data Msg = Msg
  deriving (Generic, Param)


-- Actions for that View
data MsgAction = SetMsg Text
  deriving (Generic, Param)


instance HyperView Msg where
  type Action Msg = MsgAction


-- Handle message actions
message :: Msg -> MsgAction -> Eff es (View Msg ())
message _ (SetMsg m) = do
  -- TODO - Perform action side effects
  -- re-render the view new data
  pure $ viewMsg m


-- Render a message view
viewMsg :: Text -> View Msg ()
viewMsg m = col id $ do
  el_ "Message:"
  el_ $ text m
  button (SetMsg "Goodbye") id "Goodbye"
```


Obligatory Counter Example
--------------------------

```haskell
page :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Page es Response
page var = do
  hyper $ counter var

  load $ do
    n <- readTVarIO var
    pure $ col (pad 20 . gap 10) $ do
      el h1 "Counter"
      viewId Counter (viewCount n)


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

Single Page Applications require us to write two programs: a Javascript client and a Server-side API. Hyperbole allows us instead to write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML.

There are frameworks that support this in various languages, including HTMX, Phoenix LiveView, and others. Hyperbole has the following advantages

1. 100% Haskell
2. Type safe views, actions, routes, and forms
3. Elegant interface with little boilerplate
5. Fast updates over sockets using virtual DOM 
6. Fall-back to HTTP

An Example Page
---------------

Hyperbole applications direct URL patterns to different *Page*s. We use a `load` handler for when the user first visits the page.

```haskell
module Page.Messages where
import Web.Hyperbole

-- static page
page :: (Hyperbole :> es) => Page es Response
page = do
  load $ do
    pure $ col (gap 10) $ do
      el h1 "Hello World"
```

Our top-level *Page* can be divided up into unique interactive *Views*

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
Views associate interactions from UI elements with *Actions*. We specify a server-side handlers for Actions which return new HTML. Actions can perform side effects, like reading and writing data to a database.

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
  -- look, a side effect!
  send $ SaveMessage msg
  -- use the same view function for the updated message view
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

Now that we have a page, let's create an application. First, create a sum type to specify routes

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
    -- type safe routing
    route Messages id "Messages"
```

Then create a WAI application from your router by specifying a function to turn page fragments into full web pages on first load. Make sure to include Hyperbole's embedded javascript

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







