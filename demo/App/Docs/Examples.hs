module App.Docs.Examples where

import Control.Monad (unless)
import Data.List qualified as L
import Data.String (IsString)
import App.Docs.Snippet (ModuleName (..), modulePath)
import Language.Haskell.TH
import System.Directory (doesFileExist, getCurrentDirectory)

newtype ModuleSource = ModuleSource FilePath
  deriving newtype (Show, Eq, IsString)

moduleSource :: Q Exp
moduleSource = do
  loc <- location
  let path = loc_filename loc
  localFile path

moduleSourceNamed :: ModuleName -> Q Exp
moduleSourceNamed mn = do
  let p = modulePath mn
  localFile p

localFile :: FilePath -> Q Exp
localFile p = do
  current <- runIO getCurrentDirectory
  b <- runIO $ doesFileExist p

  unless b $ do
    fail $ "Could not find file: " <> show p

  stringE $ stripDir "examples" $ stripDir current p

stripDir :: FilePath -> FilePath -> FilePath
stripDir dir p =
  maybe
    p
    (dropWhile (== '/'))
    (L.stripPrefix dir p)
