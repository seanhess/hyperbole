{-# LANGUAGE OverloadedLists #-}

module Test.ParamSpec where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Test.Syd
import Web.Hyperbole.Param
import Web.View (att)
import Web.View.Types


data Thing = Thing
  deriving (Generic, Param, Show, Eq)


data Custom = Custom
  deriving (Show, Eq)


data HasString = HasString String
  deriving (Generic, Param, Show, Eq)


data Compound
  = One
  | Two Thing
  | WithId (Id Thing)
  deriving (Generic, Param, Show, Eq)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Param, Eq, Ord)
  deriving (Generic, Show)


instance Param Custom where
  toParam Custom = "something"
  parseParam "something" = Just Custom
  parseParam _ = Nothing


spec :: Spec
spec = do
  describe "Param" $ do
    describe "toParam" $ do
      it "basic" $ toParam Thing `shouldBe` "thing"
      it "custom" $ toParam Custom `shouldBe` "something"

    describe "parseParam" $ do
      it "basic" $ parseParam "thing" `shouldBe` Just Thing
      it "basic lowercase" $ parseParam @Thing "Thing" `shouldBe` Nothing
      it "custom" $ parseParam "something" `shouldBe` Just Custom
      it "custom other" $ parseParam @Thing "custom" `shouldBe` Nothing

    describe "has-string" $ do
      it "should not contain single quotes" $ do
        toParam (HasString "woot") `shouldNotSatisfy` containsSingleQuotes

      it "should roundtrip" $ do
        let inp = HasString "woot"
        parseParam (toParam inp) `shouldBe` Just inp

    describe "compound" $ do
      it "should toparam" $ toParam (Two Thing) `shouldBe` "two-thing"
      it "double roundtrip" $ parseParam (toParam (Two Thing)) `shouldBe` Just (Two Thing)

  describe "Param Attributes" $ do
    it "should serialize basic id" $ do
      let atts = mempty :: Attributes
      (setId "woot" atts).other `shouldBe` [("id", "woot")]

    it "should serialize compound id" $ do
      let atts = mempty :: Attributes
      (setId (toParam $ Two Thing) atts).other `shouldBe` [("id", "two-thing")]

    it "should serialize stringy id" $ do
      let atts = mempty :: Attributes
      (setId (toParam $ HasString "woot") atts).other `shouldBe` [("id", "hasstring-woot")]

    it "should serialize with Id" $ do
      let atts = mempty :: Attributes
      (setId (toParam $ WithId (Id "woot")) atts).other `shouldBe` [("id", "withid-woot")]


containsSingleQuotes :: Text -> Bool
containsSingleQuotes = T.elem '\''


setId :: Text -> Mod
setId = att "id"
