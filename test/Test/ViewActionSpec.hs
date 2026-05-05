{-# LANGUAGE BlockArguments #-}

module Test.ViewActionSpec where

import Control.Exception (catch)
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
  deriving (Generic, Show, ToJSON, FromJSON, UserInput, Eq)


-- If you want another type as an input, you must implement UserInput. We don't automatically parse integers from search fields
newtype NumInput = NumInput Int
  deriving (Generic, Show, Eq)
  deriving anyclass (UserInput, FromJSON, ToJSON)


data Sum
  = SumA
  | SumB Int
  | SubC Text
  | SubD (Maybe Text)
  | SubE Term
  | SubF Something
  | SubG Int Something
  | SubH NumInput Int
  | SubI Something Int
  | SubJ Text NumInput
  deriving (Generic, Show, Eq, ViewAction)


data Compound = Compound Product
  deriving (Generic, Show, Eq, Read, ToJSON, FromEncoded, ToEncoded, FromJSON, ViewAction)


data HasText = HasText Text
  deriving (Generic, Show, Eq, Read, ViewAction, ToJSON, FromJSON, FromEncoded, ToEncoded)


newtype Term = Term Text
  deriving newtype (Eq, Show, ToJSON, FromJSON, Read)


data NumberPair = NumberPair {a :: Int, b :: NumInput}
  deriving (Show, Generic, ToJSON, FromJSON)


data Nested = Nested NumberPair
  deriving (Generic, ViewAction)


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

    withMarkers ["focus"] $ describe "toActionInput" $ do
      it "Constructor Text" $ do
        toActionInput SubC `shouldBe` Encoded "SubC" [holeArg]

      it "Constructor (Maybe Text)" $ do
        toActionInput SubD `shouldBe` Encoded "SubD" [holeArg]

      it "Constructor (Con . Just)" $ do
        catch
          do
            toActionInput (SubD . Just) `shouldBe` Encoded "SubD" [holeArg]
            failTest "Should have thrown error about constructor being too complex"
          do \ExpectedInput -> pure ()

      it "Constructor newtype Term" $ do
        toActionInput (SubE . Term) `shouldBe` Encoded "SubE" [holeArg]

      it "renders data constructors" $ do
        toActionInput SubF `shouldBe` Encoded "SubF" [holeArg]

      it "Partially applied constructors" $ do
        toActionInput (SubG 2) `shouldBe` Encoded "SubG" [JSON (Number 2), holeArg]

      it "Out-of-order constructors" $ do
        toActionInput (`SubH` 2) `shouldBe` Encoded "SubH" [holeArg, JSON (Number 2)]
        toActionInput (`SubI` 3) `shouldBe` Encoded "SubI" [holeArg, JSON (Number 3)]

      it "Nested Product" $ do
        catch
          do
            toActionInput (Nested . NumberPair 0) `shouldNotBe` Encoded "Nested" [holeArg]
            failTest "Should have thrown error about constructor being too complex"
          do \ExpectedInput -> pure ()

    -- This should be a compiler error!
    -- it "Funky Constructors" $ do
    --   catch
    --     do
    --       toActionInput (SumB . (* 10)) `shouldNotBe` Encoded "SumB" [holeArg]
    --       failTest "Should have thrown error about constructor being too complex"
    --     do \ExpectedInput -> pure ()

    withMarkers ["focus"] $ describe "encoded argument holes" $ do
      it "Constructor Text" $ do
        encodedToText (toActionInput SubC) `shouldBe` "SubC |>_<|"

      it "Constructor newtype Term" $ do
        encodedToText (toActionInput (SubE . Term)) `shouldBe` "SubE |>_<|"

      it "Partially applied constructors" $ do
        encodedToText (toActionInput (SubG 2)) `shouldBe` "SubG 2 |>_<|"

      it "Out-of-order constructors" $ do
        encodedToText (toActionInput (`SubH` 2)) `shouldBe` "SubH |>_<| 2"
        encodedToText (toActionInput (`SubI` 3)) `shouldBe` "SubI |>_<| 3"

      it "Nested Product" $ do
        -- this throws now
        -- encodedToText (toActionInput (Nested . NumberPair 0)) `shouldBe` "Nested {\"a\":12,\"b\":\"|><|\"}"
        encodedToText (toAction $ Nested $ NumberPair 12 (NumInput 34)) `shouldBe` "Nested {\"a\":12,\"b\":34}"

      it "Escapes underscores" $ do
        encodedToText (toActionInput (SubJ "hello _ world")) `shouldBe` "SubJ \"hello _ world\" |>_<|"

    -- Compiler error
    -- it "doesn't try to encode impossible constructors" $ do
    --   catch
    --     do
    --       encodedToText (toActionInput (SumB . (* 10))) `shouldNotBe` "SumB |>_<|"
    --       failTest "Should have thrown error about constructor being too complex"
    --     do \ExpectedInput -> pure ()

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
