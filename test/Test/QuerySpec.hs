{-# LANGUAGE RecordWildCards #-}

module Test.QuerySpec where

import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (urlEncode)
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Effect.Server


spec :: Spec
spec = do
  describe "param" paramSpec
  describe "render" renderSpec
  describe "multi" multiSpec
  describe "session" sessionSpec


data Woot
  deriving (Generic, Session)


sessionSpec :: Spec
sessionSpec = do
  it "should session key" $ do
    sessionKey @Woot `shouldBe` "Woot"


paramSpec :: Spec
paramSpec = do
  describe "ToParam" $ do
    it "should encode text flat" $ do
      toParam @Text "hello" `shouldBe` "hello"

    it "should encode int" $ do
      toParam @Int 23 `shouldBe` "23"

    it "should encode Maybe" $ do
      toParam @(Maybe Int) Nothing `shouldBe` ""
      toParam @(Maybe Int) (Just 23) `shouldBe` "23"

    it "should encode lists as show" $ do
      let items = ["one", "two"]
      toParam @[Text] items `shouldBe` ParamValue (cs (show items))

  describe "FromParam" $ do
    it "should parse text" $ do
      parseParam @Text "hello" `shouldBe` Right "hello"

    it "should parse int" $ do
      parseParam @Int "3" `shouldBe` Right 3

    it "should handle lists" $ do
      let items = ["one", "two", "three"] :: [Text]
      parseParam (toParam items) `shouldBe` Right items


renderSpec :: Spec
renderSpec = do
  it "should parse multiple items" $ do
    let qd = parse "msg=hello&age=1"
    require @Text "msg" qd `shouldBe` Right "hello"
    require @Int "age" qd `shouldBe` Right 1

  it "should render as a querystring" $ do
    let q =
          mempty
            & QueryData.insert @Text "msg" "value"
            & QueryData.insert @Int "age" 1
    QueryData.render q `shouldBe` "age=1&msg=value"

  it "should escape special characters in strings" $ do
    let q = mempty & QueryData.insert @Text "msg" "bob&henry=fast"
    QueryData.render q `shouldBe` "msg=bob%26henry%3Dfast"

  it "should roundtrip special characters" $ do
    let msg = "bob&henry=fast"
    let q = mempty & QueryData.insert @Text "msg" msg
    let out = QueryData.render q
    let q' = QueryData.parse out
    QueryData.lookup "msg" q' `shouldBe` Just msg

  it "should render lists" $ do
    let items = ["one", "two"]
    let q = mempty & QueryData.insert @[Text] "items" items
    QueryData.render q `shouldBe` "items=" <> urlEncode True (cs $ show items)


data Filters = Filters
  { term :: Text
  , isActive :: Bool
  , another :: Maybe Text
  }
  deriving (Eq, Show, ToParam)


instance ToQuery Filters where
  toQuery f =
    mempty
      & QueryData.insert "term" f.term
      & QueryData.insert "isActive" f.isActive
      & QueryData.insert "another" f.another


instance FromQuery Filters where
  parseQuery q = do
    term <- QueryData.require "term" q
    isActive <- QueryData.require "isActive" q
    another <- QueryData.require "another" q
    pure Filters{..}


data Nested = Nested
  { filters :: Filters
  }


instance ToQuery Nested where
  toQuery n =
    mempty & QueryData.insert "filters" n.filters


multiSpec :: Spec
multiSpec = do
  it "should convert to querydata" $ do
    let f = Filters "woot" False Nothing
    QueryData.render (toQuery f) `shouldBe` "another=&isActive=false&term=woot"

  it "should parse from querydata" $ do
    let f = Filters "woot" False Nothing
    let out = QueryData.render (toQuery f)
    let q = QueryData.parse out
    parseQuery q `shouldBe` Right f

  it "should work with Just" $ do
    let f = Filters "woot" False (Just "hello")
    let out = QueryData.render (toQuery f)
    print out
    let q = QueryData.parse out
    parseQuery q `shouldBe` Right f
