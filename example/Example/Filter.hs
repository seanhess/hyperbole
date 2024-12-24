{-# LANGUAGE UndecidableInstances #-}

module Example.Filter where

import Data.Text (Text, isInfixOf, toLower)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.View.Icon as Icon
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Prelude hiding (even, odd)


page :: (Hyperbole :> es) => Eff es (Page '[Filter])
page = do
  pure $ exampleLayout Route.Filter $ col (pad 20 . grow) $ do
    hyper Filter $ filterView allLanguages Nothing


data Filter = Filter
  deriving (Show, Read, ViewId)


type Term = Text


instance HyperView Filter es where
  data Action Filter
    = SearchTerm Text
    | Select ProgrammingLanguage
    deriving (Show, Read, ViewAction)


  update (SearchTerm term) = do
    let matched = filter (isMatchLanguage term) allLanguages
    pure $ filterView matched Nothing
  update (Select lang) = do
    pure $ filterView [] (Just lang)


filterView :: [ProgrammingLanguage] -> Maybe ProgrammingLanguage -> View Filter ()
filterView langs selected =
  col (gap 10 . grow) $ do
    stack grow $ do
      layer $ search SearchTerm 200 (placeholder "filter programming languages" . border 1 . pad 10)
    -- clearButton SearchTerm term
    chosenView selected
    resultsTable Select langs


-- It's not recommended to attempt to clear the value. Setting the value on inputs results in unexpected behavior.
-- if you need this, use a javascript component
clearButton :: (ViewAction (Action id)) => (Term -> Action id) -> Term -> Layer id ()
clearButton clear term =
  popout (offset (R 0) . pad 10 . showClearBtn) $ do
    button (clear "") (width 24 . hover (color PrimaryLight)) Icon.xCircle
 where
  showClearBtn =
    case term of
      "" -> hide
      _ -> id


chosenView :: Maybe ProgrammingLanguage -> View c ()
chosenView Nothing = none
chosenView (Just lang) = do
  row (gap 10) $ do
    el_ "You chose:"
    el_ $ text lang.name
  el (if lang.name == "Haskell" then id else hide) "You are as wise as you are attractive"


resultsTable :: (ViewAction (Action id)) => (ProgrammingLanguage -> Action id) -> [ProgrammingLanguage] -> View id ()
resultsTable onSelect langs = do
  col (gap 15) $ do
    mapM_ languageRow langs
 where
  languageRow lang = do
    col (gap 5) $ do
      row id $ do
        el bold $ text lang.name
        space
        button (onSelect lang) (pad (XY 10 2) . border 1 . hover (bg GrayLight) . rows) "Select"

      el id $ text lang.description

  rows = textAlign AlignCenter . border 1 . borderColor GrayLight


data ProgrammingLanguage = ProgrammingLanguage {name :: Text, description :: Text}
  deriving (Show, Read)
instance Eq ProgrammingLanguage where
  p1 == p2 = p1.name == p2.name


isMatchLanguage :: Text -> ProgrammingLanguage -> Bool
isMatchLanguage term p =
  isInfixOf (toLower term) . toLower $ p.name


allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ ProgrammingLanguage "JavaScript" "A versatile scripting language mainly used for web development."
  , ProgrammingLanguage "Java" "A robust, platform-independent language commonly used for enterprise applications."
  , ProgrammingLanguage "TypeScript" "A superset of JavaScript that adds static typing."
  , ProgrammingLanguage "Python" "A beginner-friendly language with a wide range of applications, from web to data science."
  , ProgrammingLanguage "PHP" "A server-side scripting language primarily used for web development."
  , ProgrammingLanguage "Go" "A statically typed, compiled language designed for simplicity and efficiency."
  , ProgrammingLanguage "C++" "A powerful language for system programming, game development, and high-performance applications."
  , ProgrammingLanguage "C#" "A language developed by Microsoft, widely used for developing Windows and web applications."
  , ProgrammingLanguage "Objective-C" "A language used primarily for macOS and iOS application development before Swift."
  , ProgrammingLanguage "Rust" "A memory-safe language focused on performance and reliability."
  , ProgrammingLanguage "Ruby" "A dynamic language known for its simplicity and used in web frameworks like Ruby on Rails."
  , ProgrammingLanguage "Swift" "A modern language for iOS and macOS application development."
  , ProgrammingLanguage "Haskell" "An elegant functional language for those with excellent taste."
  , ProgrammingLanguage "Elm" "A functional language for building reliable web front-end applications."
  , ProgrammingLanguage "Scheme" "A minimalist, functional dialect of Lisp."
  ]
