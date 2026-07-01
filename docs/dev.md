Local Development
=================


# JavaScript Client Development

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


# Haskell Server Development

Run demo locally. Then visit 

```
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
hlint .
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

Run tests, then recompile both client and server on file change, then restart demo

```
bin/dev
```
