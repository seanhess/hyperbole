module Test.ViewActionSpec where

import Data.Aeson qualified as A
import Data.Text (Text)
import GHC.Generics
import Skeletest
import Skeletest.Predicate qualified as P
import Web.Hyperbole (FromJSON, ToJSON)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.HyperView (ViewAction (..))
import Web.Hyperbole.HyperView.Event (toActionInput)


data Simple = Simple
  deriving (Generic, Eq, Show, Read, ViewAction)


data Product = Product String Int
  deriving (Generic, Show, Eq, ViewAction, Read, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Product' = Product' HasText Int
  deriving (Generic, Show, Eq, ViewAction, Read, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Sum
  = SumA
  | SumB Int
  | SubC Text
  | SubD (Maybe Text)
  | SubE Term
  deriving (Generic, Show, Read, Eq, ViewAction)


data Compound = Compound Product
  deriving (Generic, Show, Eq, Read, ToJSON, FromEncoded, ToEncoded, FromJSON, ViewAction)


data HasText = HasText Text
  deriving (Generic, Show, Eq, Read, ViewAction, ToJSON, FromJSON, FromEncoded, ToEncoded)


newtype Term = Term Text
  deriving newtype (Eq, Show, ToJSON, FromJSON, Read)


spec :: Spec
spec = do
  describe "ViewAction" $ do
    describe "toAction" $ do
      it "simple" $ toAction Simple `shouldBe` Encoded "Simple" []
      it "has text" $ toAction (HasText "hello world") `shouldBe` Encoded "HasText" ["hello world"]
      it "product" $ toAction (Product "hello world" 123) `shouldBe` Encoded "Product" ["hello world", A.Number 123]
      it "sum" $ toAction (SumB 123) `shouldBe` Encoded "SumB" [A.Number 123]
      it "compound" $ do
        let p = Product "hello world" 123
        toAction (Compound p) `shouldBe` Encoded "Compound" [A.toJSON p]

    describe "toActionInput" $ do
      it "Constructor Text" $ do
        toActionInput SubC `shouldBe` Encoded "SubC" []

      it "Constructor (Maybe Text)" $ do
        toActionInput (SubD . Just) `shouldBe` Encoded "SubD" []

      -- erm.... I guess it's a newtype so this works?
      it "Constructor newtype Term" $ do
        toActionInput (SubE . Term) `shouldBe` Encoded "SubE" []

    describe "parseAction" $ do
      it "simple" $ parseAction (Encoded "Simple" []) `shouldBe` pure Simple

      it "parse product" $ do
        parseAction @Product (Encoded "Product" ["woot", A.Number 1234]) `shouldSatisfy` P.right P.anything

      it "parse product with spaces" $ do
        parseAction @Product (Encoded "Product" ["hello world", A.Number 1234]) `shouldSatisfy` P.right P.anything

    describe "roundTrip" $ do
      it "simple" $ do
        parseAction (toAction Simple) `shouldBe` pure Simple
      it "has text multiple words" $ do
        let a = HasText "hello world"
        parseAction (toAction a) `shouldBe` pure a
      it "product" $ do
        let a = Product "hello world" 123
        parseAction @Product (toAction a) `shouldBe` pure a
      it "product'" $ do
        let a = Product' (HasText "hello world") 123
        parseAction (toAction a) `shouldBe` pure a
      it "compound" $ do
        let a = Compound (Product "hello world" 123)
        parseAction (toAction a) `shouldBe` pure a
      it "sum" $ do
        let a = SumB 123
        parseAction (toAction a) `shouldBe` pure a
