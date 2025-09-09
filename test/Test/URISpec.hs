{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.URISpec where

import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.URI


spec :: Spec
spec = do
  describe "URI" $ do
    it "should preserve slashes" $ do
      let u = [uri|http://example.com|] ./. "hello"
      u.uriPath `shouldBe` "/hello"

    it "should render with path slashes" $ do
      let u = [uri|http://example.com/test|]
      uriToText (u ./. ["wahoo"]) `shouldBe` "http://example.com/test/wahoo"
      uriToText (u ./. ["/wahoo"]) `shouldBe` "http://example.com/test/wahoo"
      uriToText (u ./. []) `shouldBe` "http://example.com/test"
      uriToText (u ./. "/") `shouldBe` "http://example.com/test"

  describe "Path" $ do
    it "handles edge cases" $ do
      path "" `shouldBe` []
      path "/" `shouldBe` []

    it "normal paths" $ do
      path "woot" `shouldBe` ["woot"]
      path "woot/hello" `shouldBe` ["woot", "hello"]
      path "/woot/hello" `shouldBe` ["woot", "hello"]
      path "/woot/hello/" `shouldBe` ["woot", "hello"]
