{-# LANGUAGE OverloadedLists #-}

module Test.ArgumentSpec where

import Data.Aeson as A
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics
import Skeletest
import Web.Hyperbole.Data.Argument


data Tag = A | B
  deriving (Generic, ToJSON, FromJSON, Eq, Show)


data Tag2 = T2 | Tag Text
  deriving (Generic, ToJSON, FromJSON, Eq, Show)


data Record = Record
  { age :: Int
  , msg :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Eq)


spec :: Spec
spec = do
  describe "ToArgument" $ do
    it "encodes basics as JSON" $ do
      encodeArgument @Text "hello" `shouldBe` "\"hello\""
      encodeArgument @Int 23 `shouldBe` "23"

    it "encodes Maybe as JSON" $ do
      encodeArgument @(Maybe Int) Nothing `shouldBe` "null"
      encodeArgument @(Maybe Int) (Just 23) `shouldBe` "23"

    it "encodes simple constructors raw" $ do
      encodeArgument A `shouldBe` "A"
      encodeArgument B `shouldBe` "B"

    it "encodes complex constructors as products" $ do
      encodeArgument T2 `shouldBe` "(T2)"
      encodeArgument (Tag "hello world") `shouldBe` "(Tag \"hello world\")"

    -- it "should encode lists with spaces = plusses" $ do
    --   encodeArgument @[Int] [1, 2, 3] `shouldBe` ParamValue ("1+2+3")
    --   encodeArgument @[Text] ["one", "two"] `shouldBe` ParamValue ("one+two")
    --   encodeArgument @[Text] ["hello world", "friend"] `shouldBe` ParamValue ("hello%20world+friend")

    it "should not escape text" $ do
      encodeArgument @Text "hello world" `shouldBe` "\"hello world\""
      encodeArgument @Text "hello_world" `shouldBe` "\"hello_world\""
      encodeArgument @Text "hello+world" `shouldBe` "\"hello+world\""

    it "encodes json" $ do
      let r = Record 10 "hello world"
      encodeArgument r `shouldBe` cs (A.encode r)

      let r2 = Record 10 "hello_world"
      encodeArgument r2 `shouldBe` cs (A.encode r2)
      encodeArgument r2 `shouldBe` cs (A.encode r2)

  describe "FromParam" $ do
    it "parses basics" $ do
      decodeArgument @Text "hello" `shouldBe` Right "hello"
      decodeArgument @Int "3" `shouldBe` Right 3

    it "decodes json" $ do
      let r2 = Record 10 "hello_world"
      decodeArgument (cs $ A.encode r2) `shouldBe` Right r2

    it "can decode numbers as text" $ do
      decodeArgument @Text "\"30\"" `shouldBe` Right "30"

    it "should not escape text" $ do
      decodeArgument @Text "\"hello world\"" `shouldBe` Right "hello world"
      decodeArgument @Text "\"hello_world\"" `shouldBe` Right "hello_world"
      decodeArgument @Text "\"hello+world\"" `shouldBe` Right "hello+world"

  describe "RoundTrip" $ do
    it "round trips constructors" $ do
      decodeArgument (encodeArgument A) `shouldBe` Right A
      decodeArgument (encodeArgument B) `shouldBe` Right B
      decodeArgument (encodeArgument T2) `shouldBe` Right T2
      let t = Tag "woo hoo"
      decodeArgument (encodeArgument t) `shouldBe` Right t
