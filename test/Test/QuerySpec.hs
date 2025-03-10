{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Test.QuerySpec where

import Data.Function ((&))
import Data.Map qualified as M
import Data.Text (Text)
import Network.HTTP.Types qualified as HTTP
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.Param as Param
import Web.Hyperbole.Data.QueryData as QueryData


spec :: Spec
spec = do
  describe "param" paramSpec
  describe "render" renderSpec
  describe "multi" multiSpec


data Woot = Woot Text
  deriving (Generic, Show)


paramSpec :: Spec
paramSpec = do
  describe "ToParam" $ do
    it "should encode basics" $ do
      toParam @Text "hello" `shouldBe` "hello"
      toParam @Int 23 `shouldBe` "23"

    it "should encode Maybe" $ do
      toParam @(Maybe Int) Nothing `shouldBe` ""
      toParam @(Maybe Int) (Just 23) `shouldBe` "23"

    -- it "should encode lists with spaces = plusses" $ do
    --   toParam @[Int] [1, 2, 3] `shouldBe` ParamValue ("1+2+3")
    --   toParam @[Text] ["one", "two"] `shouldBe` ParamValue ("one+two")
    --   toParam @[Text] ["hello world", "friend"] `shouldBe` ParamValue ("hello%20world+friend")

    it "should not escape text" $ do
      toParam @Text "hello world" `shouldBe` "hello world"

  describe "FromParam" $ do
    it "should parse basics" $ do
      parseParam @Text "hello" `shouldBe` Right "hello"
      parseParam @Int "3" `shouldBe` Right 3


-- it "should decode lists with plusses" $ do
--   parseParam @[Int] "1+2+3" `shouldBe` Right [1, 2, 3]
--
-- it "should decode lists with escapes" $ do
--   let vals = ["hello world", "friend"] :: [Text]
--   parseParam (toParam @[Text] vals) `shouldBe` Right vals

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


-- it "should preserve plusses" $ do
--   let QueryData q = QueryData $ M.fromList [("items", "one+two")]
--   print $ HTTP.toQuery $ M.toList q
--   QueryData.render (QueryData q) `shouldBe` "items=one+two"

data Filters = Filters
  { term :: Text
  , isActive :: Bool
  , another :: Maybe Text
  }
  deriving (Eq, Show)


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


data Filters' = Filters'
  { term :: Text
  , isActive :: Bool
  }
  deriving (Generic, Eq, ToJSON, FromJSON, FromParam, ToParam)
instance Default Filters' where
  def = Filters' "" False


data Nested = Nested
  { filters :: Filters'
  }
  deriving (Generic, ToQuery, FromQuery)


-- instance ToQuery Nested where
--   toQuery n =
--     mempty & QueryData.insert "filters" (JSON n.filters)
--
--
-- instance FromQuery Nested where
--   parseQuery q =
--     mempty & QueryData.insert "filters" (JSON n.filters)

multiSpec :: Spec
multiSpec = do
  it "should convert to querydata" $ do
    let f = Filters "woot" False Nothing
    QueryData.render (toQuery f) `shouldBe` "another=&isActive=false&term=woot"

  it "should convert to querydata 2" $ do
    let f = Filters "woot" False (Just "ok")
    QueryData.render (toQuery f) `shouldBe` "another=ok&isActive=false&term=woot"

  it "should parse from querydata" $ do
    let f = Filters "woot" False Nothing
    let out = QueryData.render (toQuery f)
    let q = QueryData.parse out
    parseQuery q `shouldBe` Right f

  it "should work with Just" $ do
    let f = Filters "woot" False (Just "hello")
    let out = QueryData.render (toQuery f)
    let q = QueryData.parse out
    parseQuery q `shouldBe` Right f
