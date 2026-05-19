Local Development
=================


# JavaScript Client Development

Download and install [NPM](https://nodejs.org/en/download). On a mac, can be installed via homebrew:

```
brew install npm
```

Install client dependencies

```
cd client
npm install
```

Build JavaScript client

```
npm run build
```

Watch for changes during development

```
npm run dev
```


# Haskell Server Development

Run demo locally. Then visit 

```
cabal run demo
```


### Tests

```
cabal test
```

### File watching

Run tests, then recompile both client and server on file change, then restart demo

```
bin/dev
```

### Haskell Language Server

We use a custom preprocessor to embed compiler-checked examples into Haddock. Everything is automatic with Cabal, but HLS doesn't support it yet. Run this command to get HLS working:

```
cabal install docgen:docgen --overwrite-policy=always
```
