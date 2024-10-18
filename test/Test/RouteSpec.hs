module Test.RouteSpec where

import Data.Text (Text)
import GHC.Generics
import Skeletest
import Web.Hyperbole.Route


data Routes
  = MainPage
  | Hello Hello
  | Goodbye
  deriving (Show, Generic, Eq)
instance Route Routes where
  baseRoute = Just MainPage


data Hello
  = MainHello
  | World
  | Message String
  deriving (Show, Generic, Eq)
instance Route Hello where
  baseRoute = Just MainHello


data NoMain = NoMain Nested
  deriving (Show, Generic, Eq, Route)


data Nested
  = Something
  | Nested Text
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

      it "constructors with parameters should use full url" $
        routePath (NoMain (Nested "woot")) `shouldBe` ["nomain", "nested", "woot"]

      it "no main should use full url" $
        routePath (NoMain Something) `shouldBe` ["nomain", "something"]

    describe "matchRoute" $ do
      it "basic" $ matchRoute ["goodbye"] `shouldBe` Just Goodbye
      it "default empty string" $ matchRoute [""] `shouldBe` Just MainPage
      it "default empty" $ matchRoute [] `shouldBe` Just MainPage
      it "compound" $ matchRoute ["hello", "world"] `shouldBe` Just (Hello World)
      it "compound default" $ matchRoute ["hello"] `shouldBe` Just (Hello MainHello)
      it "compound dynamic" $ matchRoute ["hello", "message", "whatever"] `shouldBe` Just (Hello (Message "whatever"))
      it "no base compound" $ matchRoute ["nomain", "nested", "hello"] `shouldBe` Just (NoMain (Nested "hello"))

    describe "baseRoute" $ do
      it "default" $ baseRoute `shouldBe` Just MainPage
      it "compound" $ (baseRoute @Hello) `shouldBe` Just MainHello
      it "none" $ (baseRoute @Nested) `shouldBe` Nothing
