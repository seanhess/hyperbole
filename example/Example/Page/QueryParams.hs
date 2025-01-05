module Example.Page.QueryParams where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Example.AppRoute qualified as Route
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Web.Hyperbole.Effect.Server (Host (..), Request (..))

data Dolphin = Dolphin
  { name :: Text,
    age :: Int
  }
  deriving (Generic, Read, FromQueryData, Show)

readDolphin :: [(Text, Maybe Text)] -> Maybe Dolphin
readDolphin query = case ((read @Int . T.unpack) <$> l "age", l "name") of
  (Just age', Just name') -> Just $ Dolphin name' age'
  _ -> Nothing
  where
    m = Map.fromList $ mapMaybe normalise query
    l k = Map.lookup k m

normalise :: (a, Maybe b) -> Maybe (a, b)
normalise (a, Just b) = Just (a, b)
normalise _ = Nothing

page :: (Hyperbole :> es) => Eff es (Page '[])
page = do
  r <- request
  pure $ exampleLayout Route.QueryParams $ do
    col (gap 10 . pad 10) $ do
      el_ $ do
        case readDolphin r.query of
          Just d -> do
            col (gap 10) $ do
              el_ $ text "successfully constructed record from query params:"
              el_ $ text $ cs $ show d
          Nothing -> col (gap 10) $ do
            el_ $ text $ cs ("failure: check query params" ++ show r.query)
            let Host host = r.host
            let queryArgs = T.intercalate "&" $ map (\(x, y) -> x <> "=" <> y) [("age", "12"), ("name", "bubu")]
            let ps = T.intercalate "/" $ "http://" : decodeUtf8 host : (r.path ++ ["?" <> queryArgs])
            let theUrl = url ps
            row (gap 10) $ do
              el_ $ text "click link to send query params:"
              el_ $ link theUrl id (text $ cs $ show theUrl)
