Hyperbole
=========

[![Hackage](https://img.shields.io/hackage/v/hyperbole.svg)](https://hackage.haskell.org/package/hyperbole)

Create fully interactive HTML applications with type-safe serverside Haskell. Inspired by [HTMX](https://htmx.org/), [Elm](https://elm-lang.org/), and [Phoenix LiveView](https://www.phoenixframework.org/)

[Learn more about Hyperbole on Hackage](https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html)

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Text (Text)
import Web.Hyperbole

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
  deriving (Generic, ViewId)

data MessageAction = Louder Text
  deriving (Generic, ViewAction)

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

Getting Started with Cabal
--------------------------

Create a new application:

    > mkdir myapp
    > cd myapp
    > cabal init

Add hyperbole and text to your build-depends:

```
    build-depends:
        base ^>=4.18.2.1
      , hyperbole
      , text
```

Paste the above example into Main.hs, and run

    > cabal run

Visit http://localhost:3000 to view the application


Examples
---------

The [example directory](https://github.com/seanhess/hyperbole/blob/main/example/README.md) contains an app with pages demonstrating various features

* [Main](https://github.com/seanhess/hyperbole/blob/main/example/Main.hs)
* [Simple](https://github.com/seanhess/hyperbole/blob/main/example/Example/Simple.hs)
* [Counter](https://github.com/seanhess/hyperbole/blob/main/example/Example/Counter.hs)
* [CSS Transitions](https://github.com/seanhess/hyperbole/blob/main/example/Example/Transitions.hs)
* [Forms](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
* [Sessions](https://github.com/seanhess/hyperbole/blob/main/example/Example/Forms.hs)
* [Redirects](https://github.com/seanhess/hyperbole/blob/main/example/Example/Redirects.hs)
* [Lazy Loading and Polling](https://github.com/seanhess/hyperbole/blob/main/example/Example/LazyLoading.hs)
* [Errors](https://github.com/seanhess/hyperbole/blob/main/example/Example/Errors.hs)
* [Live Search](https://github.com/seanhess/hyperbole/blob/main/example/Example/Search.hs)
* [Contacts (Advanced)](https://github.com/seanhess/hyperbole/blob/main/example/Example/Contacts.hs)


Learn More
----------

View Documentation on Hackage
* https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html

View on Github
* https://github.com/seanhess/hyperbole

In Production
-------------

<a href="https://nso.edu">
  <img alt="National Solar Observatory" src="https://nso1.b-cdn.net/wp-content/uploads/2020/03/NSO-logo-orange-text.png" width="400"/>
</a>

The NSO uses Hyperbole for the [L2 Data creation UI](https://github.com/DKISTDC/level2/blob/main/src/App.hs) for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/)

Local Development
-----------------

### Dependencies with Nix


With nix installed, you can use `nix develop` to get a shell with all dependencies installed.

### Manual dependency installation


Download and install [NPM](https://nodejs.org/en/download). On a mac, can be installed via homebrew:

```
brew install npm
```

Install client dependencies

```
cd client
npm install
```

Recommended: Use `direnv` to automatically load environment from .env

```
brew install direnv
direnv allow
```


### Building

Build JavaScript client

```
cd client
npx webpack
```

Run examples

```
cd example
cabal run
```

### Tests

```
cabal test
```

### File watching

Run tests, then recompile everything on file change and restart examples

```
bin/dev
```


Contributors
------------

* [Sean Hess](seanhess)
* [Kamil Figiela](https://github.com/kfigiela)
* [Christian Georgii](https://github.com/cgeorgii)
