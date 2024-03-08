module Test.ViewSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Syd
import Web.Hyperbole.HyperView


data Thing = Thing
  deriving (Show, Read, Param, Eq)


data Custom = Custom
  deriving (Show, Eq)


data HasString = HasString String
  deriving (Show, Read, Param, Eq)


data Compound
  = One
  | Two Thing
  deriving (Show, Read, Param, Eq)


instance Param Custom where
  toParam Custom = "something"
  parseParam "something" = Just Custom
  parseParam _ = Nothing


spec :: Spec
spec = do
  describe "HyperView" $ do
    describe "Param" $ do
      describe "toParam" $ do
        it "basic" $ toParam Thing `shouldBe` "Thing"
        it "custom" $ toParam Custom `shouldBe` "something"

      describe "parseParam" $ do
        it "basic" $ parseParam "Thing" `shouldBe` Just Thing
        it "basic lowercase" $ parseParam @Thing "thing" `shouldBe` Nothing
        it "custom" $ parseParam "something" `shouldBe` Just Custom
        it "custom other" $ parseParam @Thing "custom" `shouldBe` Nothing

      describe "has-string" $ do
        it "should not contain quotes" $ do
          toParam (HasString "woot") `shouldNotSatisfy` containsQuotes

        it "should roundtrip" $ do
          let inp = HasString "woot"
          parseParam (toParam inp) `shouldBe` Just inp

      describe "compound" $ do
        it "should toparam" $ toParam (Two Thing) `shouldBe` "Two Thing"
        it "double roundtrip" $ parseParam (toParam (Two Thing)) `shouldBe` Just (Two Thing)


containsQuotes :: Text -> Bool
containsQuotes = T.elem '"'
