module Example.Search where

import Data.Text (Text, isInfixOf, toLower)
import Example.Colors
import Web.Hyperbole
import Prelude hiding (even, odd)


page :: (Hyperbole :> es) => Page es '[LiveSearch]
page = do
  handle liveSearch $ load $ do
    pure $ col (pad 20) $ do
      el bold "Filter Programming Languages"
      hyper LiveSearch $ liveSearchView allLanguages


data LiveSearch = LiveSearch
  deriving (Show, Read, ViewId)


data SearchAction = GoSearch Text
  deriving (Show, Read, ViewAction)


instance HyperView LiveSearch where
  type Action LiveSearch = SearchAction


liveSearch :: LiveSearch -> SearchAction -> Eff es (View LiveSearch ())
liveSearch _ (GoSearch term) = do
  let matched = filter (isInfixOf (toLower term) . toLower) allLanguages
  pure $ liveSearchView matched


liveSearchView :: [Text] -> View LiveSearch ()
liveSearchView langs =
  col (gap 10) $ do
    search GoSearch 100 (placeholder "programming language" . border 1 . pad 10)
    resultsTable langs


resultsTable :: [Text] -> View c ()
resultsTable langs = do
  table rows langs $ do
    tcol none $ \l -> td cell $ text l
 where
  cell = pad 4
  rows = odd (bg White) . even (bg GrayLight) . textAlign Center . border 1 . borderColor GrayLight


allLanguages :: [Text]
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
