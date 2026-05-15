{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.FormSpec where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Exts (IsList (..))
import Skeletest
import Web.Hyperbole.Data.Argument
import Web.Hyperbole.HyperView.Forms
import Web.Hyperbole.HyperView.Input (InputValue (..))
import Web.Hyperbole.Types.Request


data Example f = Example
  { message :: Field f Text
  , age :: Field f Int
  , whatever :: Field f (Maybe Float)
  , maybeMessage :: Field f (Maybe Text)
  }
  deriving (Generic, FromFormF, GenFields Maybe)
instance Show (Example Identity) where
  show (Example m a w mm) = "Example " <> show m <> " " <> show a <> " " <> show w <> " " <> show mm
instance Eq (Example Identity) where
  Example m a w mm == Example m2 a2 w2 mm2 = m == m2 && a == a2 && w == w2 && mm == mm2


data Flags = Flags
  { a :: Bool
  , b :: Bool
  }
  deriving (Generic, FromForm, Show, Eq)


data Todo = Todo
  {msg :: Text}
  deriving (Generic, FromForm, Show, Eq)


instance IsList Form where
  type Item Form = Param
  fromList ps = Form ps mempty
  toList (Form ps _) = ps


spec :: Spec
spec = do
  describe "forms" $ do
    it "parses a record" $ do
      case fromForm @(Example Identity) [("message", "hello"), ("age", "23"), ("whatever", "10.4"), ("maybeMessage", "hello world")] of
        Left e -> fail $ show e
        Right a -> do
          a.message `shouldBe` "hello"
          a.age `shouldBe` 23
          a.whatever `shouldBe` Just 10.4
          a.maybeMessage `shouldBe` Just "hello world"

    it "parses empty values" $ do
      let e = fromForm @(Example Identity) [("age", ""), ("whatever", "")]
      fmap (.age) e `shouldBe` Right 0
      fmap (.whatever) e `shouldBe` Right Nothing
      fmap (.maybeMessage) e `shouldBe` Right Nothing

    it "parses missing values" $ do
      let e = fromForm @(Example Identity) []
      fmap (.age) e `shouldBe` Right 0
      fmap (.whatever) e `shouldBe` Right Nothing
      fmap (.maybeMessage) e `shouldBe` Right Nothing

    it "should parse a form with a number for the text" $ do
      let res = fromForm @(Example Identity) [("message", "30"), ("age", "0"), ("whatever", "2"), ("maybeMessage", "hello")]
      res `shouldBe` Right (Example "30" 0 (Just 2) (Just "hello"))

    it "parses missing Maybes" $ do
      let res = fromForm @(Example Identity) [("message", "30"), ("age", "0")]
      res `shouldBe` Right (Example "30" 0 Nothing Nothing)

    it "parses Maybe Text empty string" $ do
      let res = fromForm @(Example Identity) [("message", "30"), ("age", "0"), ("maybeMessage", "")]
      res `shouldBe` Right (Example "30" 0 Nothing (Just ""))

    it "parses bools" $ do
      fromForm @Flags [("a", "true"), ("b", "off")] `shouldBe` Right (Flags True False)
      fromForm @Flags [("a", "on"), ("b", "false")] `shouldBe` Right (Flags True False)
      fromForm @Flags [("a", "on")] `shouldBe` Right (Flags True False)

    it "parses missing bools as false" $ do
      fromForm @Flags [("a", "true")] `shouldBe` Right (Flags True False)

    it "parses special characters" $ do
      fromForm @Todo [("msg", "test")] `shouldBe` Right (Todo "test")
      fromForm @Todo [("msg", "hello world")] `shouldBe` Right (Todo "hello world")
      fromForm @Todo [("msg", "hello+world")] `shouldBe` Right (Todo "hello+world")
      fromForm @Todo [("msg", "hello_world")] `shouldBe` Right (Todo "hello_world")

  describe "inputs" $ do
    it "decodes option as input" $ do
      let enc = encodeArgument (Twonit "Hello world" 33)
      parseInputValue enc `shouldBe` Right (Twonit "Hello world" 33)

    it "decodes option as field" $ do
      let enc = encodeArgument (Twonit "Hello world" 33)
      fromField (Just (FormParam enc)) `shouldBe` Right (Twonit "Hello world" 33)


data FancySum
  = Unit
  | Onit Text
  | Twonit Text Int
  deriving (Generic, Eq, FromJSON, ToJSON, InputValue, FromField)
