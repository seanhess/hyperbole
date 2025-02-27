module Test.UrlSpec (spec) where

import Skeletest
import Text.Read (readMaybe)
import Web.View.Types.Url


data Something = Something Url
  deriving (Show, Read, Eq)


spec :: Spec
spec = do
  describe "Url" $ do
    describe "parsing" $ do
      it "scheme and domain" $ do
        url "https://www.google.com" `shouldBe` Url "https://" "www.google.com" [] []

      it "path urls" $ do
        url "/my/path" `shouldBe` Url "" "" ["my", "path"] []

      it "scheme, domain, and path" $ do
        url "http://woot.com/my/path" `shouldBe` Url "http://" "woot.com" ["my", "path"] []

      it "no slash prefix" $ do
        url "hello/world" `shouldBe` Url "" "" ["hello", "world"] []

      it "query" $ do
        url "/path?key=value" `shouldBe` Url "" "" ["path"] [("key", Just "value")]

    describe "render" $ do
      it "paths" $ do
        renderUrl (url "/hello/world") `shouldBe` "/hello/world"

      it "query" $ do
        renderUrl (url "/path?key=value") `shouldBe` "/path?key=value"

      it "full" $ do
        renderUrl (url "https://example.com/hello/world?hello&name=bob") `shouldBe` "https://example.com/hello/world?hello&name=bob"

      it "empty" $ do
        renderUrl (Url "" "" [] []) `shouldBe` "/"
        renderUrl (url "https://example.com/") `shouldBe` "https://example.com/"
        renderUrl (url "https://example.com") `shouldBe` "https://example.com/"

    describe "show/read" $ do
      let u = Url "" "" ["proposals"] []
      it "show" $
        show u `shouldBe` "\"/proposals\""

      it "read" $
        readMaybe "\"/proposals\"" `shouldBe` Just u

      it "show nested" $ do
        show (Something u) `shouldBe` "Something \"/proposals\""

      it "read nested" $ do
        readMaybe @Something (show (Something u)) `shouldBe` Just (Something u)
