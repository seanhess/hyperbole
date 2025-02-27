module Test.ViewSpec where

import Skeletest
import Web.View
import Web.View.Types
import Web.View.View (ViewState (..), runView)
import Prelude hiding (span)


spec :: Spec
spec = do
  describe "view" $ do
    describe "string literals" $ do
      it "should include lits at text" $ do
        let view = ("hello: " :: View c ()) >> text "world"
        (runView () view).contents `shouldBe` [Text "hello: ", Text "world"]

      it "should include text and text" $ do
        let view = text "stuff" >> text "hello"
        (runView () view).contents `shouldBe` [Text "stuff", Text "hello"]

      it "should include text and trailing lits" $ do
        let view = text "stuff" >> "hello"
        (runView () view).contents `shouldBe` [Text "stuff", Text "hello"]
