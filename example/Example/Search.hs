module Example.Search where

import Data.String (IsString)
import Data.Text (Text, isInfixOf, toLower)
import Example.Colors
import Web.Hyperbole
import Prelude hiding (even, odd)


page :: (Hyperbole :> es) => Page es '[LiveSearch]
page = do
  pure $ col (pad 20) $ do
    el bold "Filter Programming Languages"
    hyper LiveSearch $ liveSearchView allLanguages Nothing


data LiveSearch = LiveSearch
  deriving (Show, Read, ViewId)


instance HyperView LiveSearch es where
  data Action LiveSearch
    = GoSearch Text
    | Select ProgrammingLanguage
    deriving (Show, Read, ViewAction)


  handle (GoSearch term) = do
    let matched = filter (isMatchLanguage term) allLanguages
    pure $ liveSearchView matched Nothing
  handle (Select lang) = do
    pure $ liveSearchView [] (Just lang)


liveSearchView :: [ProgrammingLanguage] -> Maybe ProgrammingLanguage -> View LiveSearch ()
liveSearchView langs selected =
  col (gap 10) $ do
    search GoSearch 100 (placeholder "programming language" . border 1 . pad 10)
    chosenView selected
    resultsTable langs


chosenView :: Maybe ProgrammingLanguage -> View LiveSearch ()
chosenView Nothing = none
chosenView (Just lang) = do
  row (gap 10) $ do
    el_ "You chose:"
    el_ $ text lang.name


resultsTable :: [ProgrammingLanguage] -> View LiveSearch ()
resultsTable langs = do
  col id $ do
    mapM_ languageRow langs
 where
  languageRow :: ProgrammingLanguage -> View LiveSearch ()
  languageRow lang = do
    button (Select lang) (border 1 . hover (bg GrayLight) . rows) $ text lang.name

  rows = textAlign Center . border 1 . borderColor GrayLight


newtype ProgrammingLanguage = ProgrammingLanguage {name :: Text}
  deriving newtype (IsString, Show, Read)


isMatchLanguage :: Text -> ProgrammingLanguage -> Bool
isMatchLanguage term (ProgrammingLanguage p) =
  isInfixOf (toLower term) . toLower $ p


allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ "JavaScript"
  , "Java"
  , "TypeScript"
  , "Python"
  , "PHP"
  , "Go"
  , "C++"
  , "C#"
  , "Objective-C"
  , "Rust"
  , "Ruby"
  , "Swift"
  , "Haskell"
  , "Elm"
  , "Scheme"
  ]
