{-# LANGUAGE OverloadedLists #-}

module Test.ViewIdSpec where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.HyperView


data Thing = Thing
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded, ViewId, ToParam, FromParam)


data Custom = Custom
  deriving (Show, Eq)


data HasString = HasString String
  deriving (Generic, Show, Eq, Read, ViewId)


data Compound
  = One
  | Two Thing
  | WithId (Id Thing)
  | Compound Text Compound
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded, ViewId, ToParam, FromParam)


data Product4 = Product4 Text Text Text Text
  deriving (Generic, Show, Eq, Read, ViewId)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Eq, ToJSON, FromJSON, Ord, Show, ToParam, FromParam)
  deriving (Generic)


instance ViewId Custom where
  toViewId Custom = Encoded "something" []
  parseViewId (Encoded "something" []) = pure Custom
  parseViewId _ = Left "NOPE"


spec :: Spec
spec = withMarkers ["encoded"] $ do
  describe "ViewId Encoded" $ do
    describe "toViewId" $ do
      it "basic" $ encodeViewId Thing `shouldBe` "Thing"
      it "custom" $ encodeViewId Custom `shouldBe` "something"

    describe "parseViewId" $ do
      it "basic lowercase" $ decodeViewId @Thing "thing" `shouldBe` Nothing
      it "basic" $ decodeViewId @Thing "Thing" `shouldBe` pure Thing
      it "custom" $ decodeViewId @Custom "something" `shouldBe` pure Custom
      it "custom other" $ decodeViewId @Thing "custom" `shouldBe` Nothing

    describe "has-string" $ do
      it "should not contain single quotes" $ do
        encodeViewId (HasString "woot") `shouldBe` "HasString woot"
        containsSingleQuotes (encodeViewId (HasString "woot")) `shouldBe` False

      it "should roundtrip" $ do
        let inp = HasString "woot"
        decodeViewId (encodeViewId inp) `shouldBe` pure inp

    describe "compound" $ do
      it "double roundtrip" $ decodeViewId (encodeViewId (Two Thing)) `shouldBe` pure (Two Thing)

    describe "nested" $ do
      let nest = Compound "one" $ Compound "two" (Two Thing)
      it "should roundtrip" $ decodeViewId (encodeViewId nest) `shouldBe` pure nest

    describe "big product" $ do
      let p = Product4 "one" "two" "three" "four"
      it "should roundtrip" $ do
        let vid = encodeViewId p
        decodeViewId vid `shouldBe` pure p


-- describe "Param Attributes" $ do
--   it "should serialize basic id" $ do
--     let atts = mempty :: Attributes id
--     (setId "woot" atts).other `shouldBe` [("id", "woot")]
--
--   it "should serialize compound id" $ do
--     let atts = mempty :: Attributes id
--     (setId (toViewId $ Two Thing) atts).other `shouldBe` [("id", toViewId $ Two Thing)]
--
--   it "should serialize stringy id" $ do
--     let atts = mempty :: Attributes id
--     (setId (toViewId $ HasString "woot") atts).other `shouldBe` [("id", pack $ show $ HasString "woot")]
--
--   it "should serialize with Id" $ do
--     let atts = mempty :: Attributes id
--     (setId (toViewId $ WithId (Id "woot")) atts).other `shouldBe` [("id", "WithId \"woot\"")]

containsSingleQuotes :: Text -> Bool
containsSingleQuotes = T.elem '\''

-- setId :: Text -> Mod id
-- setId = att "id"
