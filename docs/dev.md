Local Development
=================

Download and install [NPM](https://nodejs.org/en/download). On a mac, can be installed via homebrew:

```sh
brew install npm
```

Install client dependencies

```sh
cd client
npm install
```

Recommended: Use `direnv` to automatically load environment from .env

```sh
brew install direnv
direnv allow
```


### Building

Build JavaScript client

```sh
cd client
npm run build
```

Watch for changes during development

```sh
cd client
npm run dev
```

Run examples

```
# demo needs to have demo/static and client/dist as relative paths
cd <your-path-to>/hyperbole
cabal run demo
```

### Linting and Formatting

JavaScript client:

```sh
cd client
npm run lint
npm run fmt:check
```

Haskell:

```sh
hlint src test
fourmolu --mode inplace $(git ls-files '*.hs')
```

### Tests

JavaScript client:

```sh
cd client
npm test
```

Haskell: 

```hs
cabal test
```

### File watching

Run tests, then recompile everything on file change and restart examples

```
bin/dev
```
