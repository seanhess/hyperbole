{-# LANGUAGE OverloadedLists #-}

module Test.EncodedSpec where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Skeletest
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.Param
import Web.Hyperbole.HyperView.Event (toActionInput)


-- TEST: QueryData underscores vs spaces

data One = One
  -- toJSON automatically delegates to the child's ToJSON instance
  -- when it ought to be enought to delegate to the Generic instance!
  deriving (Generic, Eq, ToEncoded, FromEncoded, ToJSON, FromJSON)


data Tag = A | B | C | D
  deriving (Generic, Eq, ToEncoded, ToJSON, FromJSON)


data Two = Two | Two2 Int
  deriving (Generic, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded)


-- -- Custom Param Encoding
-- instance ToParam Two where
--   toParam Two = "Two"
--   toParam other = genericToParam other
-- instance FromParam Two where
--   parseParam "Two" = pure Two
--   parseParam other = genericParseParam other

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
  | NProd SubProduct
  deriving (Generic, ToEncoded, FromEncoded, Eq)


data SubProduct
  = MoreThanOne Int
  | AnotherOne Text Int
  deriving (Generic, Eq, ToJSON, FromJSON)


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
      genericToEncoded (Two2 3) `shouldBe` Encoded "Two2" [JSON $ Number 3]
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toArgument One]

    it "should encode sum tags" $ do
      genericToEncoded (CTwo Two) `shouldBe` Encoded "CTwo" [toArgument Two]

    it "basic" $ do
      genericToEncoded (Gogo One) `shouldBe` Encoded "Gogo" [toArgument One]

    it "product" $ do
      genericToEncoded (Product "one" 2 True) `shouldBe` Encoded "Product" [toArgument @Text "one", toArgument @Int 2, toArgument True]

    it "product4" $ do
      let prod = Product4 "one" "two" "three" "four"
      genericToEncoded prod `shouldBe` Encoded "Product4" (fmap toArgument ["one" :: Text, "two", "three", "four"])

  describe "genericParseEncoded" $ do
    it "product4" $ do
      genericParseEncoded (Encoded "Product4" (fmap toArgument ["one" :: Text, "two", "three", "four"])) `shouldBe` Right (Product4 "one" "two" "three" "four")

    it "sum" $ do
      genericParseEncoded @Sum (Encoded "Sumthing" []) `shouldBe` Right Sumthing
      genericParseEncoded @Sum (Encoded "Num" [toArgument @Int 2]) `shouldBe` Right (Num 2)
      genericParseEncoded @Sum (Encoded "Str" [toArgument @Text "OK"]) `shouldBe` Right (Str "OK")

      genericParseEncoded @Sum (Encoded "COne" [toArgument One]) `shouldBe` Right (COne One)
      genericParseEncoded @Sum (Encoded "CTwo" [toArgument Two]) `shouldBe` Right (CTwo Two)

  describe "toEncoded" $ do
    it "encodes numbers as text" $ do
      toEncoded (Num 1) `shouldBe` Encoded "Num" [toArgument @Int 1]

  describe "toText" $ do
    it "should encode single tags" $ do
      encodedToText (Encoded "One" []) `shouldBe` "One"

  describe "parseText" $ do
    it "should decode single tags" $ do
      encodedParseText "One" `shouldBe` Right (Encoded "One" [])

    it "parses numbers" $ do
      encodedParseText "Num 1" `shouldBe` Right (Encoded "Num" [JSON $ Number 1])

  describe "encode" $ do
    it "should encode single tags" $ do
      encode One `shouldBe` "One"

    it "encodes strings as JSON" $ do
      encode (Str "hello world") `shouldBe` "Str \"hello world\""
      encode (Str " ") `shouldBe` "Str \" \""
      encode (Str "") `shouldBe` "Str \"\""
      encode (Str "_") `shouldBe` "Str \"_\""
      encode (Str "\n") `shouldBe` "Str \"\\n\""
      encode (Str "hello_world") `shouldBe` "Str \"hello_world\""
      encode (Str "hello+world") `shouldBe` "Str \"hello+world\""
      encode (Str "hello\nworld") `shouldBe` "Str \"hello\\nworld\""

    it "should encode records`" $ do
      -- no field names for ourselves
      encode (Record 1 "two") `shouldBe` "Record 1 \"two\""
      -- but if it is nested it uses the JSON instance, obviously
      let r2 = Record 1 "two"
      encode (RecordN r2) `shouldBe` "RecordN " <> encodeArgument (JSON $ toJSON r2)

    it "no special case for nested constructors`" $ do
      encode A `shouldBe` "A"
      encode (Tag A) `shouldBe` "Tag A"

    it "should encode sum" $ do
      encode (Num 1) `shouldBe` "Num 1"
      encode (Str "hello world") `shouldBe` "Str \"hello world\""

    it "should encode products" $ do
      encode (Product "hello world" 2 True) `shouldBe` "Product \"hello world\" 2 true"

    it "encodes more constructors" $ do
      encode (CTwo (Two2 3)) `shouldBe` "CTwo (Two2 3)" -- uses custom tuple constructor
      encode (CTwo Two) `shouldBe` "CTwo (Two)" -- uses the custom simpleTag
      encode (COne One) `shouldBe` "COne []"
      encode (NProd (AnotherOne "hi" 3)) `shouldBe` "NProd (AnotherOne \"hi\" 3)"

    it "encodes input holes" $ do
      encode (Str inputHole) `shouldBe` "Str _"

  describe "decode" $ do
    it "should encode single tags" $ do
      decode "One" `shouldBe` Just One

    it "should decode nested sum" $ do
      decodeEither "Num 1" `shouldBe` Right (Num 1)
      decodeEither "Str \"str\"" `shouldBe` Right (Str "str")
      decodeEither "Str \"hello world\"" `shouldBe` Right (Str "hello world")

    it "no special case for nested constructors`" $ do
      decodeEither "Tag A" `shouldBe` Right (Tag A)

    it "decodes strings" $ do
      decode "Str \"\"" `shouldBe` pure (Str "")

  describe "params" $ do
    it "encodes holes" $ do
      encodeArgument Hole `shouldBe` "_"

  -- it "sanitizeText" $ do
  --   encodeParam "hello world" `shouldBe` "hello_world"
  --   encodeParam "hello_world" `shouldBe` "hello\\_world"
  --   encodeParam "hello\nworld" `shouldBe` "hello\\nworld"
  --
  -- it "desanitizeText" $ do
  --   decodeParam "hello_world" `shouldBe` "hello world"
  --   decodeParam "hello\\_world" `shouldBe` "hello_world"
  --   decodeParam "hello\\nworld" `shouldBe` "hello\nworld"

  -- TODO: Add more edge cases to check if "\n" is escaped properly.
  -- it "edge cases" $ do
  --   encodeParam "" `shouldBe` "\"\""
  --   encodeParam " " `shouldBe` "\"_\""
  --   encodeParam "  " `shouldBe` "\"__\""
  --
  --   encodeParam "_" `shouldBe` "\"\\_\""
  --   encodeParam "__" `shouldBe` "\"\\_\\_\""
  --
  --   decodeParam "|" `shouldBe` ParamValue "|"
  --   decodeParam "_" `shouldBe` ParamValue "_"
  --   decodeParam "\\_" `shouldBe` "_"
  --   decodeParam "\\_\\_" `shouldBe` "__"

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
      decodeEither (encode (CTwo Two)) `shouldBe` Right (CTwo Two)
      decode (encode (Tag B)) `shouldBe` Just (Tag B)

    it "special case products" $ do
      decodeEither (encode (CTwo (Two2 3))) `shouldBe` Right (CTwo (Two2 3))
      decodeEither (encode (NProd (AnotherOne "HI" 3))) `shouldBe` Right (NProd (AnotherOne "HI" 3))

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
      decode @Sum (encode l) `shouldBe` Just l

    it "encodes newlines in strings" $ do
      let s = Str "hello\nworld"
      decode @Sum (encode s) `shouldBe` Just s

    -- Regression tests for https://github.com/seanhess/hyperbole/issues/187
    -- A ViewId (or state) containing a list with newline characters must
    -- encode/decode correctly.  Previously, desanitizeParamText blindly
    -- replaced the JSON escape sequence "\\n" with a real newline, corrupting
    -- the JSON and causing "No Handler for Event viewId".
    it "list with newline character round-trips correctly (issue #187)" $ do
      decode @Sum (encode (List ["\n"])) `shouldBe` Just (List ["\n"])

    it "list with newline in multiple elements" $ do
      decode @Sum (encode (List ["\n", "hello\nworld", "plain"])) `shouldBe` Just (List ["\n", "hello\nworld", "plain"])

    it "strings" $ do
      decode @Sum (encode (Str "")) `shouldBe` pure (Str "")
      decode @Sum (encode (Str " ")) `shouldBe` pure (Str " ")
      decode @Sum (encode (Str "_")) `shouldBe` pure (Str "_")
      decode @Sum (encode (Str "~")) `shouldBe` pure (Str "~")
      decode @Sum (encode (Str "+")) `shouldBe` pure (Str "+")
      decode @Sum (encode (Str "hello world")) `shouldBe` pure (Str "hello world")
      decode @Sum (encode (Str "hello_world")) `shouldBe` pure (Str "hello_world")
