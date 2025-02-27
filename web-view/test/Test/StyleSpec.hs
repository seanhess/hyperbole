module Test.StyleSpec (spec) where

import Data.Map.Strict qualified as M
import Skeletest
import Web.View
import Web.View.Style ((-.))
import Web.View.Types (Attributes (..), Class (..), selector)
import Prelude hiding (span)


spec :: Spec
spec = do
  describe "Style Class" $ do
    it "should compile, and set both the className and styles" $ do
      let as = list Decimal mempty
      length (M.elems as.classes) `shouldBe` 1
      [c] <- pure $ M.elems as.classes
      c.selector `shouldBe` selector "list-decimal"
      c.properties `shouldBe` M.fromList [("list-style-type", "decimal")]

    it "should work with outside member None" $ do
      let as = list None mempty
      length (M.elems as.classes) `shouldBe` 1
      [c] <- pure $ M.elems as.classes
      c.selector `shouldBe` selector "list-none"
      c.properties `shouldBe` M.fromList [("list-style-type", "none")]

  describe "ToClassName" $ do
    it "should hyphenate classnames" $ do
      "woot" -. None `shouldBe` "woot-none"
    it "should not hyphenate with empty suffix" $ do
      "woot" -. () `shouldBe` "woot"
