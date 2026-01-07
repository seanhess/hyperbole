module Docs.FindExamples where

import Control.Monad (forM)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath

-- embedFiles :: FilePath -> Q Exp
-- embedFiles folder = _

allExamples :: IO [FilePath]
allExamples = findModules "examples/Example"

findModules :: FilePath -> IO [FilePath]
findModules dir = do
  fs <- listDirectory dir
  allFiles <- mapM (checkFile dir) fs
  pure $ mconcat allFiles

checkFile :: FilePath -> FilePath -> IO [FilePath]
checkFile dir f
  | takeExtension f == ".hs" = pure [fullpath]
  | otherwise = do
      isDir <- doesDirectoryExist fullpath
      if isDir
        then findModules fullpath
        else pure []
 where
  fullpath = dir </> f

-- if isDir

-- let path = dir </> f
-- if isDir
--   then findModules path
--   else
--     if takeExtension f == ".hs"
--       then pure [abspath]
--       else pure []
--
-- embedSource path (isTopLevel tld) (isCurrentDefinition tld)
