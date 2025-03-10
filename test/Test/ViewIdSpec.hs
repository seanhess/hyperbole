{-# LANGUAGE OverloadedLists #-}

module Test.ViewIdSpec where

import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Generics
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.HyperView
import Web.View.Types


data Thing = Thing
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded, ViewId)


data Custom = Custom
  deriving (Show, Eq)


data HasString = HasString String
  deriving (Generic, Show, Eq, Read, ViewId)


data Compound
  = One
  | Two Thing
  | WithId (Id Thing)
  | Compound Text Compound
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToEncoded, FromEncoded, ViewId)


data Product4 = Product4 Text Text Text Text
  deriving (Generic, Show, Eq, Read, ViewId)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)
  deriving (Generic)


instance ViewId Custom where
  toViewId Custom = "something"
  parseViewId "something" = Just Custom
  parseViewId _ = Nothing


spec :: Spec
spec = do
  describe "ViewId" $ do
    describe "toViewId" $ do
      it "basic" $ toViewId Thing `shouldBe` "Thing"
      it "custom" $ toViewId Custom `shouldBe` "something"

    describe "parseViewId" $ do
      it "basic lowercase" $ parseViewId @Thing "thing" `shouldBe` Nothing
      it "basic" $ parseViewId @Thing "Thing" `shouldBe` Just Thing
      it "custom" $ parseViewId "something" `shouldBe` Just Custom
      it "custom other" $ parseViewId @Thing "custom" `shouldBe` Nothing

    describe "has-string" $ do
      it "should not contain single quotes" $ do
        toViewId (HasString "woot") `shouldBe` "HasString \"woot\""
        containsSingleQuotes (toViewId (HasString "woot")) `shouldBe` False

      it "should roundtrip" $ do
        let inp = HasString "woot"
        parseViewId (toViewId inp) `shouldBe` Just inp

    describe "compound" $ do
      it "double roundtrip" $ parseViewId (toViewId (Two Thing)) `shouldBe` Just (Two Thing)

    describe "nested" $ do
      let nest = Compound "one" $ Compound "two" (Two Thing)
      it "should roundtrip" $ parseViewId (toViewId nest) `shouldBe` Just nest

    describe "big product" $ do
      let p = Product4 "one" "two" "three" "four"
      it "should roundtrip" $ do
        let vid = toViewId p
        parseViewId vid `shouldBe` Just p

  describe "Param Attributes" $ do
    it "should serialize basic id" $ do
      let atts = mempty :: Attributes id
      (setId "woot" atts).other `shouldBe` [("id", "woot")]

    it "should serialize compound id" $ do
      let atts = mempty :: Attributes id
      (setId (toViewId $ Two Thing) atts).other `shouldBe` [("id", toViewId $ Two Thing)]

    it "should serialize stringy id" $ do
      let atts = mempty :: Attributes id
      (setId (toViewId $ HasString "woot") atts).other `shouldBe` [("id", pack $ show $ HasString "woot")]

    it "should serialize with Id" $ do
      let atts = mempty :: Attributes id
      (setId (toViewId $ WithId (Id "woot")) atts).other `shouldBe` [("id", "WithId \"woot\"")]


containsSingleQuotes :: Text -> Bool
containsSingleQuotes = T.elem '\''


setId :: Text -> Mod id
setId = att "id"
