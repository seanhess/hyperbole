{-# LANGUAGE TemplateHaskell #-}

module Example.Tags where

import App.Docs
import App.Route qualified as Route
import Data.Text (Text)
import Example.Style.Cyber (btn)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Tags]
page = do
  pure $ layout (Route.Examples Route.Tags) $ do
    example $(moduleSource) $ do
      hyper Tags $ tagsView []

newtype Tag = Tag Text
  deriving newtype (ToParam, FromParam, Eq)

data TagForm = TagForm
  { tag :: Text
  }
  deriving (Generic, FromForm)

data Tags = Tags
  deriving (Generic, ViewId)

instance HyperView Tags es where
  data Action Tags
    = SubmitTag [Tag]
    | RemoveTag [Tag] Tag
    deriving (Generic, ViewAction)

  update (SubmitTag ts) = do
    TagForm t <- formData
    pure $ tagsView (Tag t : ts)
  update (RemoveTag ts t) = do
    pure $ tagsView $ filter (/= t) ts

tagsView :: [Tag] -> View Tags ()
tagsView ts = do
  row ~ gap 5 $ do
    mapM_ (tagView ts) ts

  form (SubmitTag ts) ~ gap 10 . pad 10 . flexRow $ do
    field "tag" ~ grow $ do
      label $ do
        input TextInput @ placeholder "New Tag" ~ border 1 . pad 10 @ value ""
    submit "+ Add" ~ btn

tagView :: [Tag] -> Tag -> View Tags ()
tagView ts (Tag t) = do
  row ~ border 1 . pad 5 . gap 5 $ do
    button (RemoveTag ts (Tag t)) ~ pad 2 . btn $ "X"
    text t
