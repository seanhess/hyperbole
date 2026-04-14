module Test.ViewSpec where

import Skeletest
import Web.Hyperbole


spec :: Spec
spec = do
  describe "View" $ do
    describe "monad" $ do
      it "renders all nodes with do" $ do
        let v = do
              el "one"
              el "two"
        renderText v `shouldBe` "<div>one</div>\n<div>two</div>"

      it "renders all nodes with >>" $ do
        let v = el "one" >> el "two"
        renderText v `shouldBe` "<div>one</div>\n<div>two</div>"

      it "renders all nodes with >>=" $ do
        let v = el "one" >>= \_ -> el "two"
        renderText v `shouldBe` "<div>one</div>\n<div>two</div>"
