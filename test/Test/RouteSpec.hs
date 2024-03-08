module Test.RouteSpec (spec) where

import GHC.Generics
import Test.Syd
import Web.Hyperbole.Route


data Routes
  = MainPage
  | Hello Hello
  | Goodbye
  deriving (Show, Generic, Eq, Route)


data Hello
  = MainHello
  | World
  | Message String
  deriving (Show, Generic, Eq, Route)


spec :: Spec
spec = do
  describe "Route" $ do
    describe "routePath" $ do
      it "basic" $
        routePath Goodbye `shouldBe` ["goodbye"]

      it "default" $
        routePath MainPage `shouldBe` []

      it "dynamic" $
        routePath (Hello (Message "woot")) `shouldBe` ["hello", "message", "woot"]

      it "compound" $
        routePath (Hello World) `shouldBe` ["hello", "world"]

      it "compound default" $
        routePath (Hello MainHello) `shouldBe` ["hello"]

    describe "matchRoute" $ do
      it "basic" $ matchRoute ["goodbye"] `shouldBe` Just Goodbye
      it "default empty string" $ matchRoute [""] `shouldBe` Just MainPage
      it "default empty" $ matchRoute [] `shouldBe` Just MainPage
      it "compound" $ matchRoute ["hello", "world"] `shouldBe` Just (Hello World)
      it "compound default" $ matchRoute ["hello"] `shouldBe` Just (Hello MainHello)
      it "compound dynamic" $ matchRoute ["hello", "message", "whatever"] `shouldBe` Just (Hello (Message "whatever"))

    describe "defRoute" $ do
      it "default" $ defRoute `shouldBe` MainPage
      it "compound" $ (defRoute :: Hello) `shouldBe` MainHello
