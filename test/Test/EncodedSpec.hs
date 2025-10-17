{-# LANGUAGE OverloadedLists #-}

module Test.EncodedSpec where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Skeletest
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.Param


-- TEST: QueryData underscores vs spaces

data One = One
  -- toJSON automatically delegates to the child's ToJSON instance
  -- when it ought to be enought to delegate to the Generic instance!
  deriving (Generic, Eq, ToEncoded, FromEncoded, ToParam, FromParam)


data Tag = A | B | C | D
  deriving (Generic, Eq, ToEncoded, ToParam, FromParam)


data Two = Two | Two2 Int
  deriving (Generic, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded)


-- Custom Param Encoding
instance ToParam Two where
  toParam Two = "Two"
  toParam other = genericToParam other
instance FromParam Two where
  parseParam "Two" = pure Two
  parseParam other = genericParseParam other


data Sum
  = Sumthing
  | Num Int
  | Str Text
  | COne One
  | CTwo Two
  | List [Text]
  deriving (Generic, Eq, ToEncoded, FromEncoded)


data Nested
  = Gogo One
  | RecordN Record
  | RecordEx Record Int
  | Tag Tag
  deriving (Generic, ToEncoded, FromEncoded, Eq)


data Product
  = Product Text Int Bool
  deriving (Generic, Eq, ToEncoded, FromEncoded)


data Record = Record
  { one :: Int
  , two :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq, ToEncoded, FromEncoded, ToParam, FromParam)


data Product4 = Product4 Text Text Text Text deriving (Generic, Show, Eq, Read, FromEncoded, ToEncoded)


spec :: Spec
spec = withMarkers ["encoded"] $ do
  describe "genericToEncoded" $ do
    it "should encode single tags" $ do
      genericToEncoded One `shouldBe` Encoded "One" []

    it "should encode multi tags" $ do
      genericToEncoded Two `shouldBe` Encoded "Two" []
      genericToEncoded (Two2 3) `shouldBe` Encoded "Two2" [jsonParam $ Number 3]
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toParam One]

    it "should encode sum tags" $ do
      genericToEncoded (CTwo Two) `shouldBe` Encoded "CTwo" [toParam Two]

    it "basic" $ do
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toParam One]

    it "product" $ do
      genericToEncoded (Product "one" 2 True) `shouldBe` Encoded "Product" [toParam @Text "one", toParam @Int 2, toParam True]

    it "product4" $ do
      let prod = Product4 "one" "two" "three" "four"
      genericToEncoded prod `shouldBe` Encoded "Product4" (fmap toParam ["one" :: Text, "two", "three", "four"])

  describe "genericParseEncoded" $ do
    it "product4" $ do
      genericParseEncoded (Encoded "Product4" (fmap toParam ["one" :: Text, "two", "three", "four"])) `shouldBe` Right (Product4 "one" "two" "three" "four")

    it "sum" $ do
      genericParseEncoded @Sum (Encoded "Sumthing" []) `shouldBe` Right Sumthing
      genericParseEncoded @Sum (Encoded "Num" [toParam @Int 2]) `shouldBe` Right (Num 2)
      genericParseEncoded @Sum (Encoded "Str" [toParam @Text "OK"]) `shouldBe` Right (Str "OK")

      genericParseEncoded @Sum (Encoded "COne" [toParam One]) `shouldBe` Right (COne One)
      genericParseEncoded @Sum (Encoded "CTwo" [toParam Two]) `shouldBe` Right (CTwo Two)

  describe "toEncoded" $ do
    it "encodes numbers as text" $ do
      -- no, this is right, but when we go to decode, we pick up the json instance...
      toEncoded (Num 1) `shouldBe` Encoded "Num" [jsonParam $ Number 1]

  describe "toText" $ do
    it "should encode single tags" $ do
      encodedToText (Encoded "One" []) `shouldBe` "One"

  describe "parseText" $ do
    it "should decode single tags" $ do
      encodedParseText "One" `shouldBe` Right (Encoded "One" [])

    it "parses numbers" $ do
      encodedParseText "Num 1" `shouldBe` Right (Encoded "Num" [jsonParam $ Number 1])

  describe "encode" $ do
    it "should encode single tags" $ do
      encode One `shouldBe` "One"

    it "encodes strings" $ do
      encode (Str "hello world") `shouldBe` "Str hello_world"
      -- but then how is it going to know the difference between the two?
      encode (Str " ") `shouldBe` "Str _"
      encode (Str "") `shouldBe` "Str |"
      encode (Str "_") `shouldBe` "Str \\_"
      encode (Str "\n") `shouldBe` "Str \\n"
      encode (Str "hello_world") `shouldBe` "Str hello\\_world"
      encode (Str "hello+world") `shouldBe` "Str hello+world"
      encode (Str "hello\nworld") `shouldBe` "Str hello\\nworld"

    it "should encode records`" $ do
      -- no field names for ourselves
      encode (Record 1 "two") `shouldBe` "Record 1 two"
      -- but if it is nested it uses the JSON instance, obviously
      let r2 = Record 1 "two"
      encode (RecordN r2) `shouldBe` "RecordN " <> encodeParam (jsonParam r2)

    it "no special case for nested constructors`" $ do
      encode A `shouldBe` "A"
      encode (Tag A) `shouldBe` "Tag A"

    it "should encode sum" $ do
      encode (Num 1) `shouldBe` "Num 1"
      encode (Str "hello world") `shouldBe` "Str hello_world"

    it "should encode prodcuts" $ do
      encode (Product "hello world" 2 True) `shouldBe` "Product hello_world 2 true"

    it "encodes more constructors" $ do
      encode (CTwo (Two2 3)) `shouldBe` "CTwo [\"Two2\",3]"
      encode (CTwo Two) `shouldBe` "CTwo Two" -- uses the custom toparam instance
      encode (COne One) `shouldBe` "COne []"

  describe "decode" $ do
    it "should encode single tags" $ do
      decode "One" `shouldBe` Just One

    it "should decode nested sum" $ do
      decodeEither "Num 1" `shouldBe` Right (Num 1)
      decodeEither "Str str" `shouldBe` Right (Str "str")
      decodeEither "Str hello_world" `shouldBe` Right (Str "hello world")

    it "no special case for nested constructors`" $ do
      decode "Tag A" `shouldBe` Just (Tag A)

    it "decodes strings" $ do
      decode "Str |" `shouldBe` pure (Str "")

  describe "params" $ do
    it "sanitizeText" $ do
      encodeParam "hello world" `shouldBe` "hello_world"
      encodeParam "hello_world" `shouldBe` "hello\\_world"
      encodeParam "hello\nworld" `shouldBe` "hello\\nworld"

    it "desanitizeText" $ do
      decodeParam "hello_world" `shouldBe` "hello world"
      decodeParam "hello\\_world" `shouldBe` "hello_world"
      decodeParam "hello\\nworld" `shouldBe` "hello\nworld"

    -- TODO: Add more edge cases to check if "\n" is escaped properly.
    it "edge cases" $ do
      encodeParam "" `shouldBe` "|"
      encodeParam " " `shouldBe` "_"
      encodeParam "  " `shouldBe` "__"

      encodeParam "_" `shouldBe` "\\_"
      encodeParam "__" `shouldBe` "\\_\\_"

      decodeParam "|" `shouldBe` ""
      decodeParam "_" `shouldBe` " "
      decodeParam "\\_" `shouldBe` "_"
      decodeParam "\\_\\_" `shouldBe` "__"

  describe "round trip" $ do
    it "records" $ do
      let enc = genericToEncoded (Record 1 "two")
      genericParseEncoded enc `shouldBe` Right (Record 1 "two")

    it "product" $ do
      decode (encode (Product "hello world" 2 False)) `shouldBe` Just (Product "hello world" 2 False)
      decode (encode (Product "bob" (-2) True)) `shouldBe` Just (Product "bob" (-2) True)

    it "nested product with records" $ do
      let r = RecordEx (Record 2 "three") 33
      let t = encode r
      decode t `shouldBe` Just r

    it "special case constructors" $ do
      decode (encode (CTwo Two)) `shouldBe` Just (CTwo Two)
      decode (encode (Tag B)) `shouldBe` Just (Tag B)

    it "big product" $ do
      let p = Product4 "hello world" "two_times" "three" "four"
      decode (encode p) `shouldBe` Just p

    it "empty strings" $ do
      decode (encode $ Str "") `shouldBe` Just (Str "")

    it "special characters" $ do
      let str = "hello+world \"bob_lives\""
      decode (encode $ Str str) `shouldBe` Just (Str str)

    it "encodes lists`" $ do
      let l = List ["hello, world", "", "+,|<[]"]
      print $ encode l
      decode @Sum (encode l) `shouldBe` Just l

    it "strings" $ do
      decode @Sum (encode (Str "")) `shouldBe` pure (Str "")
      decode @Sum (encode (Str " ")) `shouldBe` pure (Str " ")
      decode @Sum (encode (Str "_")) `shouldBe` pure (Str "_")
      decode @Sum (encode (Str "~")) `shouldBe` pure (Str "~")
      decode @Sum (encode (Str "+")) `shouldBe` pure (Str "+")
      decode @Sum (encode (Str "hello world")) `shouldBe` pure (Str "hello world")
      decode @Sum (encode (Str "hello_world")) `shouldBe` pure (Str "hello_world")
