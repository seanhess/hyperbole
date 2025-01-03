{-# LANGUAGE OverloadedLists #-}

module Test.FormSpec where

import Data.Text (Text)
import Skeletest
import Web.Hyperbole.Effect.QueryData
import Web.Hyperbole.View.Forms


data Example f = Example
  { message :: Field f Text
  , age :: Field f Int
  , whatever :: Field f (Maybe Float)
  }
  deriving (Generic)
instance Form Example Maybe


spec :: Spec
spec = do
  describe "QueryData" $ do
    it "should parse text" $ do
      parseQueryData @Text "hello" `shouldBe` Right "hello"

    it "should parse int" $ do
      parseQueryData @Int "3" `shouldBe` Right 3

    it "should handle lists" $ do
      let items = ["one", "two", "three"] :: [Text]
      parseQueryData (toQueryData items) `shouldBe` Right items

  describe "forms" $ do
    it "should parse a form" $ do
      case formParse @Example [("message", "hello"), ("age", "23"), ("whatever", "")] of
        Left e -> fail $ show e
        Right a -> do
          a.message `shouldBe` "hello"
          a.age `shouldBe` 23
          a.whatever `shouldBe` Nothing
