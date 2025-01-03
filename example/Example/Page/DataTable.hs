{-# LANGUAGE LambdaCase #-}

module Example.Page.DataTable where

import Data.List (sortOn)
import Data.Text (pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages)
import Example.View.Layout (exampleLayout)
import Example.View.SortableTable (dataTable, sortColumn)
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[Languages])
page = do
  pure $ exampleLayout Route.DataTable $ col (pad 20 . grow) $ do
    hyper Languages $ languagesView allLanguages

data Languages = Languages
  deriving (Show, Read, ViewId)

data SortField
  = SortName
  | SortDescription
  | SortFamily
  deriving (Show, Read)

instance HyperView Languages es where
  data Action Languages
    = SortOn SortField
    deriving (Show, Read, ViewAction)

  update (SortOn fld) = do
    let sorted = sortOnField fld allLanguages
    pure $ languagesView sorted

sortOnField :: SortField -> [ProgrammingLanguage] -> [ProgrammingLanguage]
sortOnField = \case
  SortName -> sortOn (.name)
  SortDescription -> sortOn (.description)
  SortFamily -> sortOn (.family)

languagesView :: [ProgrammingLanguage] -> View Languages ()
languagesView langs =
  table dataTable langs $ do
    sortColumn "Language" (SortOn SortName) (.name)
    sortColumn "Family" (SortOn SortFamily) $ \d -> pack $ show d.family
    sortColumn "Description" (SortOn SortDescription) (.description)
