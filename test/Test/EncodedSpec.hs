{-# LANGUAGE OverloadedLists #-}

module Test.EncodedSpec where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson qualified as A
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Skeletest
import Web.Hyperbole.Data.Encoded


data One = One
  -- toJSON automatically delegates to the child's ToJSON instance
  -- when it ought to be enought to delegate to the Generic instance!
  deriving (Generic, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Tag = A | B | C | D
  deriving (Generic, ToJSON, Eq, FromJSON, ToEncoded)


data Two = Two | Two2 Int
  deriving (Generic, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Sum
  = Sumthing
  | Num Int
  | Str Text
  | COne One
  | CTwo Two
  deriving (Generic, Eq, ToEncoded, FromEncoded)


data Nested
  = Gogo One
  | RecordN Record
  | RecordEx Record Int
  | Tag Tag
  deriving (Generic, ToEncoded, FromEncoded, Eq)


data Product
  = Product Text Int Text
  deriving (Generic, Eq, ToEncoded, FromEncoded)


data Record = Record
  { one :: Int
  , two :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Eq, ToEncoded, FromEncoded)


data Product4 = Product4 Text Text Text Text deriving (Generic, Show, Eq, Read, FromEncoded, ToEncoded)


spec :: Spec
spec = withMarkers ["focus"] $ do
  describe "genericToEncoded" $ do
    it "should encode single tags" $ do
      genericToEncoded One `shouldBe` Encoded "One" []

    it "should encode multi tags" $ do
      genericToEncoded Two `shouldBe` Encoded "Two" []
      genericToEncoded (Two2 3) `shouldBe` Encoded "Two2" [Number 3]
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toJSON One]

    it "should encode sum tags" $ do
      genericToEncoded (CTwo Two) `shouldBe` Encoded "CTwo" [toJSON Two]

    it "basic" $ do
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toJSON One]

    it "product" $ do
      genericToEncoded (Product "one" 2 "three") `shouldBe` Encoded "Product" [String "one", Number 2, String "three"]

    it "product4" $ do
      let prod = Product4 "one" "two" "three" "four"
      genericToEncoded prod `shouldBe` Encoded "Product4" (fmap String ["one", "two", "three", "four"])

  describe "genericParseEncoded" $ do
    it "product4" $ do
      genericParseEncoded (Encoded "Product4" (fmap String ["one", "two", "three", "four"])) `shouldBe` Right (Product4 "one" "two" "three" "four")

    it "sum" $ do
      genericParseEncoded @Sum (Encoded "Sumthing" []) `shouldBe` Right (Sumthing)
      genericParseEncoded @Sum (Encoded "Num" [Number 2]) `shouldBe` Right (Num 2)
      genericParseEncoded @Sum (Encoded "Str" [String "OK"]) `shouldBe` Right (Str "OK")

      genericParseEncoded @Sum (Encoded "COne" [toJSON One]) `shouldBe` Right (COne One)
      genericParseEncoded @Sum (Encoded "CTwo" [toJSON Two]) `shouldBe` Right (CTwo Two)

  describe "toText" $ do
    it "should encode single tags" $ do
      encodedToText (Encoded "One" []) `shouldBe` "One"

  describe "parseText" $ do
    it "should decode single tags" $ do
      encodedParseText "One" `shouldBe` Right (Encoded "One" [])

  describe "encode" $ do
    it "should encode single tags" $ do
      encode One `shouldBe` "One"

    it "should encode records`" $ do
      -- no field names for ourselves
      encode (Record 1 "two") `shouldBe` ("Record 1 \"two\"")
      -- but if it is nested it uses the JSON instance, obviously
      encode (RecordN $ Record 1 "two") `shouldBe` "RecordN " <> (cs $ A.encode $ Record 1 "two")

    it "no special case for nested constructors`" $ do
      encode A `shouldBe` "A"
      encode (Tag A) `shouldBe` "Tag \"A\""

    it "should encode sum" $ do
      encode (Num 1) `shouldBe` "Num 1"
      encode (Str "hello world") `shouldBe` "Str \"hello world\""

    it "should encode nullary constructors" $ do
      encode (CTwo (Two2 3)) `shouldBe` "CTwo " <> cs (A.encode (Two2 3))
      encode (CTwo Two) `shouldBe` "CTwo " <> cs (A.encode Two)
      encode (COne One) `shouldBe` "COne " <> cs (A.encode One)

  describe "decode" $ do
    it "should encode single tags" $ do
      decode "One" `shouldBe` Just One

    it "should decode nested sum" $ do
      decode "Num 1" `shouldBe` Just (Num 1)
      decode "Str \"str\"" `shouldBe` Just (Str "str")

    it "special case for nested constructors`" $ do
      decode "Tag \"A\"" `shouldBe` Just (Tag A)
      decode @Nested "Tag A" `shouldBe` Nothing

  describe "round trip" $ do
    it "records" $ do
      let enc = genericToEncoded (Record 1 "two")
      genericParseEncoded enc `shouldBe` Right (Record 1 "two")

    it "nested product with records" $ do
      let r = RecordEx (Record 2 "three") 33
      let t = encode r
      print t
      decode t `shouldBe` Just r

    it "special case constructors" $ do
      decode (encode (CTwo Two)) `shouldBe` Just (CTwo Two)
      decode (encode (Tag B)) `shouldBe` Just (Tag B)

    it "big product" $ do
      let p = Product4 "one" "two" "three" "four"
      decode (encode p) `shouldBe` Just p
