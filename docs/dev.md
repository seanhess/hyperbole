Local Development
=================

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
# demo needs to have demo/static and client/dist as relative paths
cd <your-path-to>/hyperbole
cabal run demo
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
