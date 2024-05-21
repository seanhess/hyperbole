Hyperbole
=========

[![Hackage](https://img.shields.io/hackage/v/hyperbole.svg)](https://hackage.haskell.org/package/hyperbole) 

Create fully interactive HTML applications with type-safe serverside Haskell. Inspired by [HTMX](https://htmx.org/), [Elm](https://elm-lang.org/), and [Phoenix LiveView](https://www.phoenixframework.org/)

[Learn more about Hyperbole on Hackage](https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html)

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (page mainPage)


mainPage = do
  handle message
  load $ do
    pure $ do
      el bold "My Page"
      hyper (Message 1) $ messageView "Hello"
      hyper (Message 2) $ messageView "World!"


data Message = Message Int
  deriving (Generic, Param)

data MessageAction = Louder Text
  deriving (Generic, Param)

instance HyperView Message where
  type Action Message = MessageAction


message :: Message -> MessageAction -> Eff es (View Message ())
message _ (Louder m) = do
  let new = m <> "!"
  pure $ messageView new


messageView m = do
  el_ $ text m
  button (Louder m) id "Louder"
```

Examples
---------

[The example directory](/example/README.md) contains an app with pages demonstrating different features

Run the examples in this repo using cabal. Then visit http://localhost:3000/ in your browser

```
cabal run
```
* [Main](./Main.hs)
* [Simple](./Example/Simple.hs)
* [Counter](./Example/Counter.hs)
* [CSS Transitions](./Example/Transitions.hs)
* [Forms](./Example/Forms.hs)
* [Sessions](./Example/Forms.hs)
* [Redirects](./Example/Redirects.hs)
* [Lazy Loading and Polling](./Example/LazyLoading.hs)
* [Errors](./Example/Errors.hs)
* [Contacts (Advanced)](./Example/Contacts.hs)

In Production
-------------

Hyperbole is used in production by the [National Solar Observatory](https://nso.edu/)
