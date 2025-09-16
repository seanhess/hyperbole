{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Test.QuerySpec where

import Data.Function ((&))
import Data.Text (Text)
import Skeletest
import Skeletest.Predicate qualified as P
import Web.Hyperbole hiding (Number)
import Web.Hyperbole.Data.QueryData as QueryData


spec :: Spec
spec = withMarkers ["param"] $ do
  describe "render" renderSpec
  describe "class" classSpec
  describe "multi" multiSpec


data Woot = Woot Text
  deriving (Generic, Show)


data Record = Record
  { age :: Int
  , msg :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToParam, FromParam, Eq, FromQuery, ToQuery)


classSpec :: Spec
classSpec = do
  describe "FromQuery" $ do
    it "decodes record" $ do
      let qd = QueryData.parse "age=20&msg=hello_world"
      parseQuery @Record qd `shouldSatisfy` P.right P.anything

    it "decodes numbers as text if needed" $ do
      let qd = QueryData.parse "age=20&msg=30"
      parseQuery @Record qd `shouldBe` Right (Record 20 "30")

  describe "ToQuery" $ do
    it "encodes record" $ do
      let r = Record 20 "hello world"
      QueryData.render (toQuery r) `shouldBe` "age=20&msg=hello_world"

  describe "roundtrip" $ do
    it "round trips" $ do
      let r = Record 20 "hello world"
      parseQuery (toQuery r) `shouldBe` Right r


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

  -- it "handles underscores" $ do
  --   QueryData.render [(Param "msg", ParamValue "hello_world" $ String "hello_world")] `shouldBe` "msg=hello%5C_world"
  --   QueryData.render [(Param "msg", ParamValue "hello world" $ String "hello world")] `shouldBe` "msg=hello_world"

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
  describe "Roundtrip" $ do
    it "should parse from querydata" $ do
      let f = Filters "hello world" False Nothing
      let out = QueryData.render (toQuery f)
      let q = QueryData.parse out
      parseQuery q `shouldBe` Right f

    it "should work with Just" $ do
      let f = Filters "hello_world" False (Just "hello")
      let out = QueryData.render (toQuery f)
      let q = QueryData.parse out
      parseQuery q `shouldBe` Right f
