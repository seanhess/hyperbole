{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Test.FormSpec where

import Data.Text (Text)
import Skeletest
import Web.Hyperbole.HyperView.Forms


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


spec :: Spec
spec = withMarkers ["param"] $ do
  describe "forms" $ do
    it "should parse a form" $ do
      case fromForm @(Example Identity) [("message", "hello"), ("age", "23"), ("whatever", "")] of
        Left e -> fail $ show e
        Right a -> do
          a.message `shouldBe` "hello"
          a.age `shouldBe` 23
          a.whatever `shouldBe` Nothing

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
      fromForm @Flags [("a", "true"), ("b", "false")] `shouldBe` Right (Flags True False)

    it "parses missing bools as false" $ do
      fromForm @Flags [("a", "true")] `shouldBe` Right (Flags True False)

    it "parses underscores" $ do
      fromForm @Todo [("msg", "test")] `shouldBe` Right (Todo "test")
      fromForm @Todo [("msg", "hello world")] `shouldBe` Right (Todo "hello world")
      fromForm @Todo [("msg", "hello+world")] `shouldBe` Right (Todo "hello+world")
      fromForm @Todo [("msg", "hello_world")] `shouldBe` Right (Todo "hello_world")
