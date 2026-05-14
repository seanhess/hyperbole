module Web.Hyperbole.Effect.Request where

import Effectful
import Web.Hyperbole.Data.URI (Path (..))
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Response (parseError)
import Web.Hyperbole.HyperView.Input (InputValue (..))
import Web.Hyperbole.Types.Request


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es Path
reqPath = (.path) <$> request


{- | Return the request body as a 'Form'

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Hyperbole :> es) => Eff es Form
formBody = do
  (.form) <$> request


inputValue :: (InputValue a, Hyperbole :> es) => Eff es a
inputValue = do
  t <- (.input) <$> request
  case parseInputValue t of
    Left err -> parseError $ "Input: " <> err
    Right a -> pure a
