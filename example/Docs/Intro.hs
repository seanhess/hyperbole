{-# LANGUAGE TemplateHaskell #-}

module Docs.Intro where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.FileEmbed
import Data.String.Conversions (cs)
import Data.Text (pack)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.AppRoute as Route
import Example.Style qualified as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole as Hyperbole


findTopLevel :: ByteString -> [ByteString] -> [ByteString]
findTopLevel definition source =
  let rest = dropWhile (not . isTopLevel definition) source
   in takeWhile (not . isBlankLine) rest
 where
  isTopLevel def line = BS.isPrefixOf def line
  isBlankLine line = BS.null $ BS.dropSpace line


page :: (Hyperbole :> es) => Page es '[]
page = do
  pure $ exampleLayout (Docs Route.Intro) $ do
    col (pad 20 . gap 10) $ do
      el bold "HYPERBOLE"
      el_ "First, we split an application into pages, each of which is completely independent"

      -- just need to embed a source file as a certain point
      sample $ findTopLevel "simplePage" (BS.lines simple)
 where
  simple :: ByteString
  simple = $(embedFile "Example/Simple.hs")

  sample :: [ByteString] -> View c ()
  sample lns =
    pre Style.code $ do
      cs $ BS.unlines lns


messagePage = do
  run 3000 $ do
    liveApp (basicDocument "Example") (page messagePage)


main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (page messagePage)



-- data Counter = Counter
--   deriving (Show, Read, ViewId)
--
--
-- instance (Reader (TVar Int) :> es, Concurrent :> es) => HyperView Counter es where
--   data Action Counter
--     = Increment
--     | Decrement
--     deriving (Show, Read, ViewAction)
--
--
--   update Increment = do
--     n <- modify (+ 1)
--     pure $ viewCount n
--   update Decrement = do
--     n <- modify (subtract 1)
--     pure $ viewCount n
--
--
-- viewCount :: Int -> View Counter ()
-- viewCount n = col (gap 10) $ do
--   row id $ do
--     el (bold . fontSize 48 . border 1 . pad (XY 20 0)) $ text $ pack $ show n
--   row (gap 10) $ do
--     button Decrement Style.btn "Decrement"
--     button Increment Style.btn "Increment"
--
--
-- modify :: (Concurrent :> es, Reader (TVar Int) :> es) => (Int -> Int) -> Eff es Int
-- modify f = do
--   var <- ask
--   atomically $ do
--     modifyTVar var f
--     readTVar var
--
--
-- getCount :: (Concurrent :> es, Reader (TVar Int) :> es) => Eff es Int
-- getCount = readTVarIO =<< ask
--
--
-- initCounter :: (Concurrent :> es) => Eff es (TVar Int)
-- initCounter = newTVarIO 0
