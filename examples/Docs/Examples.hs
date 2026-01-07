module Docs.Examples where

import Data.String (IsString)
import Docs.Snippet (ModuleName (..), modulePath)
import Language.Haskell.TH
import System.Directory (doesFileExist)

newtype ExampleSource = ExampleSource FilePath
  deriving newtype (Show, Eq, IsString)

exampleSource :: Q Exp
exampleSource = do
  loc <- location
  stringE (loc_filename loc)

moduleSource :: ModuleName -> Q Exp
moduleSource mn = do
  let p = modulePath mn
  -- attempt to open the file
  b <- runIO $ doesFileExist p
  if b
    then stringE p
    else fail $ "Could not find module: " <> show mn
