![Hyperbole](example/static/logo-robot.png)

[![Hackage](https://img.shields.io/hackage/v/hyperbole.svg?color=success)](https://hackage.haskell.org/package/hyperbole)

Create interactive HTML applications with type-safe serverside Haskell. Inspired by [HTMX](https://htmx.org/), [Elm](https://elm-lang.org/), and [Phoenix LiveView](https://www.phoenixframework.org/)

[Learn more about Hyperbole on Hackage](https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html)

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Text (Text)
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)


page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"


data Message = Message1 | Message2
  deriving (Show, Read, ViewId)


instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ messageView new


messageView :: Text -> View Message ()
messageView msg = do
  row id $ do
    button (Louder msg) id "Louder"
    el_ $ text msg
```

Getting Started with Cabal
--------------------------

Create a new application:

    $ mkdir myapp
    $ cd myapp
    $ cabal init

Add hyperbole and text to your build-depends:

```
    build-depends:
        base
      , hyperbole
      , text
```

Paste the above example into Main.hs, and run

    $ cabal run

Visit http://localhost:3000 to view the application


Examples
---------

The example directory contains an app demonstrating various features. See it in action at https://docs.hyperbole.live

<a href="https://docs.hyperbole.live">
  <img alt="Hyperbole Examples" src="example/static/examples.png"/>
</a>


Learn More
----------

View Documentation on Hackage
* https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html

View on Github
* https://github.com/seanhess/hyperbole


Full Production Example
-----------------------

<a href="https://nso.edu">
  <img alt="National Solar Observatory" src="https://nso1.b-cdn.net/wp-content/uploads/2020/03/NSO-logo-orange-text.png" width="400"/>
</a>

The NSO uses Hyperbole for the Level 2 Data creation tool for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/). It is completely [open source](https://github.com/DKISTDC/level2/). This production application contains complex interfaces, workers, databases, and more.


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
* [Pfalzgraf Martin](https://github.com/Skyfold)
* [Tushar Adhatrao](https://github.com/tusharad)
