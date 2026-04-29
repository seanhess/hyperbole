module Test.ViewActionSpec where

import Data.Aeson (Value (..))
import Data.Text (Text)
import GHC.Generics
import Skeletest
import Skeletest.Predicate qualified as P
import Web.Hyperbole (FromJSON, ToJSON)
import Web.Hyperbole.Data.Argument
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.View


data Simple = Simple
  deriving (Generic, Eq, Show, ViewAction)


data Product = Product String Int
  deriving (Generic, Show, Eq, ViewAction, Read, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Product' = Product' HasText Int
  deriving (Generic, Show, Eq, ViewAction, Read, ToJSON, FromJSON, ToEncoded, FromEncoded)


data Something = Something
  deriving (Generic, ToJSON, FromJSON, Show, Eq)


data Sum
  = SumA
  | SumB Int
  | SubC Text
  | SubD (Maybe Text)
  | SubE Term
  | SubF Something
  | SubG Int Something
  | SubH Int Int
  | SubI Something Int
  deriving (Generic, Show, Eq, ViewAction)


data Compound = Compound Product
  deriving (Generic, Show, Eq, Read, ToJSON, FromEncoded, ToEncoded, FromJSON, ViewAction)


data HasText = HasText Text
  deriving (Generic, Show, Eq, Read, ViewAction, ToJSON, FromJSON, FromEncoded, ToEncoded)


newtype Term = Term Text
  deriving newtype (Eq, Show, ToJSON, FromJSON, Read)


spec :: Spec
spec = withMarkers ["encoded"] $ do
  describe "ViewAction" $ do
    describe "toAction" $ do
      it "simple" $ toAction Simple `shouldBe` Encoded "Simple" []
      it "has text" $ toAction (HasText "hello world") `shouldBe` Encoded "HasText" [JSON $ String "hello world"]
      it "product" $ toAction (Product "hello world" 123) `shouldBe` Encoded "Product" [JSON $ String "hello world", toArgument @Int 123]
      it "sum" $ toAction (SumB 123) `shouldBe` Encoded "SumB" [toArgument @Int 123]
      it "compound" $ do
        let p = Product "hello world" 123
        toAction (Compound p) `shouldBe` Encoded "Compound" [toArgument p]

    describe "toActionInput" $ do
      it "Constructor Text" $ do
        toActionInput SubC `shouldBe` Encoded "SubC" [Hole]

      it "Constructor (Maybe Text)" $ do
        toActionInput (SubD . Just) `shouldBe` Encoded "SubD" [Hole]

      it "Constructor newtype Term" $ do
        toActionInput (SubE . Term) `shouldBe` Encoded "SubE" [Hole]

      it "renders data constructors" $ do
        toActionInput SubF `shouldBe` Encoded "SubF" [Hole]

      it "Partially applied constructors" $ do
        toActionInput (SubG 2) `shouldBe` Encoded "SubG" [JSON (Number 2), Hole]

      it "Out-of-order constructors" $ do
        toActionInput (`SubH` 2) `shouldBe` Encoded "SubH" [Hole, JSON (Number 2)]
        toActionInput (`SubI` 3) `shouldBe` Encoded "SubI" [Hole, JSON (Number 3)]

    describe "parseAction" $ do
      it "simple" $ parseAction (Encoded "Simple" []) `shouldBe` pure Simple

      it "parse product" $ do
        parseAction @Product (Encoded "Product" [JSON $ String "woot", toArgument @Int 1234]) `shouldSatisfy` P.right P.anything

      it "parse product with spaces" $ do
        parseAction @Product (Encoded "Product" [JSON $ String "hello world", toArgument @Int 1234]) `shouldSatisfy` P.right P.anything

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
