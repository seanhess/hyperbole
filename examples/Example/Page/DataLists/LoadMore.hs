module Example.Page.DataLists.LoadMore where

import Docs.Page
import Effectful
import Example.AppRoute as Route
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages)
import Example.Page.DataLists.Filter (viewFamily)
import Example.Style.Cyber (btn)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Page es '[Languages]
page = do
  ls <- loadNextLanguages 0
  pure $ layout (Data LoadMore) $ do
    example (Data LoadMore) $ do
      el "Progressively load more data"
      col ~ embed $ hyper (Languages 0) $ languagesView ls

type Offset = Int

-- fake database load of next N language
loadNextLanguages :: Offset -> Eff es [ProgrammingLanguage]
loadNextLanguages offset =
  pure $ fmap snd $ filter isInPage $ zip [0 ..] allLanguages
 where
  isInPage (n, _) = n >= offset && n < offset + nextLanguagesPageSize

nextLanguagesPageSize :: Int
nextLanguagesPageSize = 4

data Languages = Languages Offset
  deriving (Generic, ViewId)

instance HyperView Languages es where
  data Action Languages
    = Load
    deriving (Generic, ViewAction)

  update Load = do
    Languages offset <- viewId
    ls <- loadNextLanguages offset
    pure $ languagesView ls

languagesView :: [ProgrammingLanguage] -> View Languages ()
languagesView ls = do
  col ~ gap 20 $ do
    mapM_ languageView ls
  col ~ pad (TRBL 20 0 0 0) $ do
    nextLanguages ls

nextLanguages :: [ProgrammingLanguage] -> View Languages ()
nextLanguages ls
  | length ls < nextLanguagesPageSize = pure ()
  | otherwise = do
      Languages off <- viewId
      hyper (Languages (off + nextLanguagesPageSize)) $ do
        button Load ~ btn $ "Load More"

languageView :: ProgrammingLanguage -> View Languages ()
languageView lang = do
  col ~ gap 6 $ do
    row $ do
      el ~ bold $ text lang.name
      space
      row $ viewFamily lang.family
    el $ text lang.description
