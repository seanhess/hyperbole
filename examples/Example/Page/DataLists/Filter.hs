{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.DataLists.Filter where

import Data.Text (Text, pack)
import Effectful hiding (Dynamic)
import Example.AppRoute as Route
import Example.Colors
import Example.Data.ProgrammingLanguage (LanguageFamily (..), ProgrammingLanguage (..), TypeFeature (..), allLanguages, isMatchLanguage)
import Example.View.Icon as Icon
import Example.View.Inputs (toggleCheckbox)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es, IOE :> es) => Page es '[Languages]
page = do
  filters <- query
  pure $ exampleLayout (Data Filter) $ do
    example (Data Filter) $ do
      el "Incrementally search a list of data, storing parameters in the query string"
      el ~ embed $ hyper Languages $ languagesView filters

data Languages = Languages
  deriving (Generic, ViewId)

-- Filters available from the query
-- See Example.Data.ProgrammingLanguage
data Filters = Filters
  { features :: [TypeFeature]
  , family :: Maybe LanguageFamily
  , term :: Text
  }
  deriving (Generic, Show, FromQuery, ToQuery)

instance (IOE :> es) => HyperView Languages es where
  data Action Languages
    = SearchTerm Text
    | Select ProgrammingLanguage
    | Feature TypeFeature Bool
    | SetFamily (Maybe LanguageFamily)
    deriving (Generic, ViewAction)

  update = \case
    Select lang -> do
      pure $ chosenView lang
    SearchTerm term -> do
      filters <- modFilters $ \f -> f{term}
      pure $ languagesView filters
    Feature feature selected -> do
      filters <- modFilters $ \f -> setFeatures feature selected f
      pure $ languagesView filters
    SetFamily f -> do
      filters <- modFilters $ \Filters{features, term} -> Filters{family = f, features, term}
      pure $ languagesView filters
   where
    setFeatures feature selected Filters{term, family, features} =
      let features' = if selected then addFeature feature features else delFeature feature features
       in Filters{term, family, features = features'}
    addFeature f fs
      | f `elem` fs = fs
      | otherwise = f : fs
    delFeature feature =
      filter (/= feature)
    modFilters f = do
      filts <- query
      let filts' = f filts
      setQuery filts'
      pure filts'

-- apply our filters, return any languages that match
filterLanguages :: Filters -> [ProgrammingLanguage]
filterLanguages filts =
  filter match allLanguages
 where
  match lang =
    isMatchLanguage filts.term lang
      && matchFamily filts.family lang
      && matchFeatures filts.features lang
  matchFamily Nothing _ = True
  matchFamily (Just fam) lang = lang.family == fam
  matchFeatures feats lang =
    all (\f -> f `elem` lang.features) feats

languagesView :: Filters -> View Languages ()
languagesView filters = do
  let matched = filterLanguages filters
  col ~ gap 10 . grow $ do
    filtersView filters
    resultsTable Select matched

filtersView :: Filters -> View Languages ()
filtersView filters = do
  el ~ stack . grow $ do
    search SearchTerm 250 @ placeholder "filter programming languages" . value filters.term . autofocus ~ border 1 . pad 10
    clearButton SearchTerm filters.term

  row $ do
    col ~ gap 5 $ do
      el ~ bold $ "Language Family"
      familyDropdown filters
    space
    col ~ gap 5 $ do
      el ~ bold $ "Type System Features"
      feature Dynamic
      feature Typed
      feature Generics
      feature TypeClasses
      feature TypeFamilies
 where
  feature f =
    row ~ gap 10 $ do
      toggleCheckbox (Feature f) (f `elem` filters.features)
      el $ text (featureName f)

  featureName f = pack $ show f

familyDropdown :: Filters -> View Languages ()
familyDropdown filters =
  dropdown SetFamily filters.family ~ border 1 . pad 10 $ do
    option Nothing "Any"
    option (Just ObjectOriented) "Object Oriented"
    option (Just Functional) "Functional"

clearButton :: (ViewAction (Action id)) => (Text -> Action id) -> Text -> View id ()
clearButton clear term =
  el ~ popup (R 0) . pad 10 . showClearBtn $ do
    button (clear "") ~ width 24 . hover (color PrimaryLight) $ Icon.xCircle
 where
  showClearBtn =
    case term of
      "" -> display None
      _ -> id

chosenView :: ProgrammingLanguage -> View c ()
chosenView lang = do
  row ~ gap 10 $ do
    el "You chose:"
    el $ text lang.name
  el ~ (if lang.name == "Haskell" then id else display None) $ "You are as wise as you are attractive"

resultsTable :: (ViewAction (Action id)) => (ProgrammingLanguage -> Action id) -> [ProgrammingLanguage] -> View id ()
resultsTable onSelect langs = do
  col ~ gap 15 $ do
    mapM_ languageRow langs
 where
  languageRow lang = do
    col ~ gap 5 $ do
      row ~ gap 5 $ do
        el ~ bold $ text lang.name
        space
        button (onSelect lang) ~ pad (XY 10 2) . border 1 . hover (bg GrayLight) . rows $ "Select"

      row $ viewFamily lang.family

      row ~ gap 5 $ do
        el $ text lang.description

  rows = textAlign AlignCenter . border 1 . borderColor GrayLight

viewFamily :: LanguageFamily -> View c ()
viewFamily fam = do
  el ~ bg Light . pad (XY 10 2) . fontSize 16 . textAlign AlignCenter $ family fam
 where
  family Functional = "Functional"
  family ObjectOriented = "Object Oriented"
