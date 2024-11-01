module Test.ViewActionSpec where

import Data.Text (Text)
import GHC.Generics
import Skeletest
import Web.Hyperbole.HyperView


data Simple = Simple
  deriving (Generic, Eq, Show, Read, ViewAction)


data Product = Product String Int
  deriving (Generic, Show, Eq, ViewAction, Read)


data Product' = Product' HasText Int
  deriving (Generic, Show, Eq, ViewAction, Read)


data Sum
  = SumA
  | SumB Int
  deriving (Generic, Show, Eq, Read, ViewAction)


data Compound = Compound Product
  deriving (Generic, Show, Eq, Read, ViewAction)


data HasText = HasText Text
  deriving (Generic, Show, Eq, Read, ViewAction)


-- data Compound
--   = One
--   | Two Thing
--   | WithId (Id Thing)
--   deriving (Generic, Param, Show, Eq)
--
--
-- newtype Id a = Id {fromId :: Text}
--   deriving newtype (Param, Eq, Ord)
--   deriving (Generic, Show)
--
--
-- instance Param Custom where
--   toParam Custom = "something"
--   parseParam "something" = Just Custom
--   parseParam _ = Nothing

spec :: Spec
spec = do
  describe "ViewAction" $ do
    describe "toAction" $ do
      it "simple" $ toAction Simple `shouldBe` "Simple"
      it "has text" $ toAction (HasText "hello world") `shouldBe` "HasText \"hello world\""
      it "product" $ toAction (Product "hello world" 123) `shouldBe` "Product \"hello world\" 123"
      it "sum" $ toAction (SumB 123) `shouldBe` "SumB 123"
      it "compound" $ toAction (Compound (Product "hello world" 123)) `shouldBe` "Compound (Product \"hello world\" 123)"

    describe "parseAction" $ do
      it "simple" $ parseAction "Simple" `shouldBe` Just Simple

    describe "roundTrip" $ do
      it "simple" $ do
        parseAction (toAction Simple) `shouldBe` Just Simple
      it "has text multiple words" $ do
        let a = HasText "hello world"
        parseAction (toAction a) `shouldBe` Just a
      it "product" $ do
        let a = Product "hello world" 123
        parseAction (toAction a) `shouldBe` Just a
      it "product'" $ do
        let a = Product' (HasText "hello world") 123
        parseAction (toAction a) `shouldBe` Just a
      it "compound" $ do
        let a = Compound (Product "hello world" 123)
        parseAction (toAction a) `shouldBe` Just a
      it "sum" $ do
        let a = SumB 123
        parseAction (toAction a) `shouldBe` Just a
