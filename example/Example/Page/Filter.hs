{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Filter where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Data.ProgrammingLanguage (LanguageFamily (..), ProgrammingLanguage (..), allLanguages, isMatchLanguage)
import Example.View.Icon as Icon
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[Filter])
page = do
  term <- fromMaybe "" <$> lookupParam "term"
  let matched = filter (isMatchLanguage term) allLanguages

  pure $ exampleLayout Route.Filter $ col (pad 20 . grow) $ do
    hyper Filter $ filterView matched Nothing

data Filter = Filter
  deriving (Show, Read, ViewId)

type Term = Text

instance HyperView Filter es where
  data Action Filter
    = SearchTerm Text
    | Select ProgrammingLanguage
    deriving (Show, Read, ViewAction)

  update (SearchTerm term) = do
    -- save the search term as a query param
    setParam "term" term

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
      row (gap 5) $ do
        el bold $ text lang.name
        space
        button (onSelect lang) (pad (XY 10 2) . border 1 . hover (bg GrayLight) . rows) "Select"

      row id $ do
        el (bg Light . pad (XY 10 2) . fontSize 16 . textAlign AlignCenter) (family lang.family)

      row (gap 5) $ do
        el id $ text lang.description

  family Functional = "Functional"
  family ObjectOriented = "Object Oriented"

  rows = textAlign AlignCenter . border 1 . borderColor GrayLight
