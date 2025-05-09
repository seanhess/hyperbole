{-# LANGUAGE OverloadedLists #-}

module Test.FormSpec where

import Data.Text (Text)
import Skeletest
import Web.Hyperbole.HyperView.Forms


data Example f = Example
  { message :: Field f Text
  , age :: Field f Int
  , whatever :: Field f (Maybe Float)
  }
  deriving (Generic, FromFormF, GenFields Maybe)


spec :: Spec
spec = do
  describe "forms" $ do
    it "should parse a form" $ do
      case fromForm @(Example Identity) [("message", "hello"), ("age", "23"), ("whatever", "")] of
        Left e -> fail $ show e
        Right a -> do
          a.message `shouldBe` "hello"
          a.age `shouldBe` 23
          a.whatever `shouldBe` Nothing
