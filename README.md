![Hyperbole](https://github.com/seanhess/hyperbole/raw/main/demo/static/logo-robot.png)

[![Hackage Version](https://img.shields.io/hackage/v/hyperbole?color=success)](https://hackage.haskell.org/package/hyperbole)

Create interactive HTML applications with type-safe serverside Haskell. Inspired by HTMX, Elm, and Phoenix LiveView.

[▶️ Simple Example](https://hyperbole.live/simple)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)

page :: (Hyperbole :> es) => Page es '[Message]
page = do
  pure $ do
    hyper Message1 ~ bold $ messageView "Hello"
    hyper Message2 ~ bold $ messageView "World!"

data Message = Message1 | Message2
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView msg = do
  button (Louder msg) ~ border 1 $ text msg
```



Examples
---------

The examples directory contains an app demonstrating many features. See them in action at [hyperbole.live](https://hyperbole.live)

<a href="https://hyperbole.live">
  <img alt="Hyperbole Examples" src="https://github.com/seanhess/hyperbole/raw/main/demo/static/examples.png"/>
</a>

<!-- out of date!
* [HaskRead](https://github.com/tusharad/Reddit-Clone-Haskell) - A Reddit Clone
-->

Getting Started with Cabal
--------------------------

Create a new application:

    $ mkdir myapp
    $ cd myapp
    $ cabal init

Add hyperbole and text as dependencies to the `.cabal` file:

```
    build-depends:
        base
      , hyperbole
      , text

    default-language: GHC2021
```

Paste the above example into Main.hs, and run:

    $ cabal run

Visit http://localhost:3000 to view the application


Learn More
----------

<a href="https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html" target="_blank" style="border-radius: 20px; Background-color:#f8f8f8; gap: 20px; display: flex; flex-direction: row; align-items: center">
    <img src="https://github.com/seanhess/hyperbole/raw/main/docs/hackage.svg">
</a>

* [Using NIX](./docs/nix.md)
* [Local Development](./docs/dev.md)
* [Comparison with Similar Frameworks](./docs/comparison.md)

In the Wild
---------------------

<a href="https://nso.edu">
  <img alt="National Solar Observatory" src="https://nso1.b-cdn.net/wp-content/uploads/2020/03/NSO-logo-orange-text.png" width="400"/>
</a>

The NSO uses Hyperbole to manage Level 2 Data pipelines for the [DKIST telescope](https://nso.edu/telescopes/dki-solar-telescope/). It uses complex user interfaces, workers, databases, and more. [The entire codebase is open source](https://github.com/DKISTDC/level2/). 






Contributors
------------

* [Sean Hess](seanhess)
* [Kamil Figiela](https://github.com/kfigiela)
* [Christian Georgii](https://github.com/cgeorgii)
* [Pfalzgraf Martin](https://github.com/Skyfold)
* [Tushar Adhatrao](https://github.com/tusharad)
* [Benjamin Thomas](https://github.com/benjamin-thomas)
* [Adithya Obilisetty](https://github.com/adithyaov)
