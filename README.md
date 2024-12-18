Hyperbole
=========

[![Hackage](https://img.shields.io/hackage/v/hyperbole.svg?color=success)](https://hackage.haskell.org/package/hyperbole)

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

### Try Example Project with Nix

If you want to get a feel for hyperbole without cloning the project run `nix run github:seanhess/hyperbole` to run the example webserver locally

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

### Nix

Prepend targets with ghc982 or ghc966 to use GHC 9.8.2 or GHC 9.6.6

- `nix run` starts the example project with GHC 9.8.2
- `nix develop` to get a shell with all dependencies installed for GHC 9.8.2. 
- `nix develop .#ghc966-hyperbole` for GHC 9.6.6

You can also get a development shell for the example project with:

```
cd example
nix develop ../#ghc982-example
cabal run
```

You can import this flake's overlay to add `hyperbole` to all package sets and override ghc966 and ghc982 with the packages to satisfy `hyperbole`'s dependencies.

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    hyperbole.url = "github:seanhess/hyperbole"; # or "path:/path/to/cloned/hyperbole";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, hyperbole, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ hyperbole.overlays.default ];
        };
        haskellPackagesOverride = pkgs.haskell.packages.ghc966.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev: {
            # your overrides here
          });
        });
      in
      {
        devShells.default = haskellPackagesOverride.shellFor {
          packages = p: [ p.hyperbole ];
        };
      }
    );
}
```

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
