{-# LANGUAGE OverloadedLists #-}

module Test.ParamSpec where

import Data.Aeson
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics
import Skeletest
import Web.Hyperbole.Data.Param


spec :: Spec
spec = withMarkers ["param"] $ do
  describe "param" paramSpec


data Record = Record
  { age :: Int
  , msg :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToParam, FromParam, Eq)


data Tag = A | B
  deriving (Generic, ToParam, FromParam, Eq, Show)


data Tag2 = C | Tag Text
  deriving (Generic, ToParam, FromParam, Eq, Show)
instance ToJSON Tag2 where
  toJSON = genericToJSON jsonOptions


paramSpec :: Spec
paramSpec = do
  describe "ToParam" $ do
    it "should encode basics" $ do
      toParam @Text "hello" `shouldBe` "hello"
      toParam @Int 23 `shouldBe` ParamValue "23"

    it "should encode Maybe" $ do
      toParam @(Maybe Int) Nothing `shouldBe` ParamValue "~"
      toParam @(Maybe Int) (Just 23) `shouldBe` ParamValue "23"

    it "encodes simple constructors" $ do
      toParam A `shouldBe` ParamValue "A"
      toParam B `shouldBe` ParamValue "B"

    it "encodes complex constructors as json" $ do
      toParam C `shouldBe` jsonParam C
      toParam (Tag "hello world") `shouldBe` jsonParam (Tag "hello world")

    -- it "should encode lists with spaces = plusses" $ do
    --   toParam @[Int] [1, 2, 3] `shouldBe` ParamValue ("1+2+3")
    --   toParam @[Text] ["one", "two"] `shouldBe` ParamValue ("one+two")
    --   toParam @[Text] ["hello world", "friend"] `shouldBe` ParamValue ("hello%20world+friend")

    it "should not escape text" $ do
      toParam @Text "hello world" `shouldBe` ParamValue "hello world"
      toParam @Text "hello_world" `shouldBe` ParamValue "hello_world"
      toParam @Text "hello+world" `shouldBe` ParamValue "hello+world"

    it "encodes json" $ do
      let r = Record 10 "hello world"
      toParam r `shouldBe` jsonParam (toJSON r)

      let r2 = Record 10 "hello_world"
      toParam r2 `shouldBe` jsonParam (toJSON r2)
      toParam r2 `shouldBe` ParamValue (cs (encode r2))

  describe "FromParam" $ do
    it "should parse basics" $ do
      parseParam @Text "hello" `shouldBe` Right "hello"
      parseParam @Int "3" `shouldBe` Right 3

    it "decodes json" $ do
      let r2 = Record 10 "hello_world"
      parseParam (jsonParam r2) `shouldBe` Right r2
      parseParam (ParamValue $ cs $ encode r2) `shouldBe` Right r2

    it "can decode numbers as text" $ do
      parseParam @Text "3" `shouldBe` Right "3"

    it "should not escape text" $ do
      parseParam @Text "hello world" `shouldBe` Right "hello world"
      parseParam @Text "hello_world" `shouldBe` Right "hello_world"
      parseParam @Text "hello+world" `shouldBe` Right "hello+world"

  describe "RoundTrip" $ do
    it "round trips constructors" $ do
      parseParam (toParam A) `shouldBe` Right A
      parseParam (toParam B) `shouldBe` Right B
      parseParam (toParam C) `shouldBe` Right C
      let t = Tag "woo hoo"
      parseParam (toParam t) `shouldBe` Right t
