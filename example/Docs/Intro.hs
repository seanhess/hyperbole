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
  isTopLevel = BS.isPrefixOf
  isBlankLine line = BS.null $ BS.dropSpace line


page :: (Hyperbole :> es) => Eff es (Page '[])
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
