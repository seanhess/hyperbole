module Docs.Examples where

import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Docs.Snippet (ModuleName (..), modulePath)
import Language.Haskell.TH
import System.Directory (doesFileExist, getCurrentDirectory)

newtype ExampleSource = ExampleSource FilePath
  deriving newtype (Show, Eq, IsString)

exampleSource :: Q Exp
exampleSource = do
  loc <- location
  dir <- runIO getCurrentDirectory
  let path = loc_filename loc
  stringE $ stripDir "/examples" $ stripDir dir $ path
 where
  stripDir dir p =
    fromMaybe p $
      L.stripPrefix dir p

moduleSource :: ModuleName -> Q Exp
moduleSource mn = do
  let p = modulePath mn
  -- attempt to open the file
  b <- runIO $ doesFileExist p
  if b
    then stringE p
    else fail $ "Could not find module: " <> show mn
