
module Example.Page.Tags where

import Data.Text (Text)
import Example.AppRoute qualified as Route
import Example.Style.Cyber (btn)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Test]
page = do
  pure $ exampleLayout Route.Test $ do
    example Route.Test $ do
      col ~ embed $ do
        hyper Test $ tagsView []

newtype Tag = Tag Text
  deriving newtype (ToParam, FromParam)

data TagForm = TagForm
  { tag :: Text
  }
  deriving (Generic, FromForm)

data Test = Test
  deriving (Generic, ViewId)

instance  HyperView Test es where
  data Action Test
    = SubmitTag [Tag]
    deriving (Generic, ViewAction)

  update (SubmitTag ts) = do
    TagForm t <- formData
    pure $ tagsView (Tag t : ts)

tagsView :: [Tag] -> View Test ()
tagsView ts = do
  row ~ gap 5 $ do
    mapM_ tagView ts

  form (SubmitTag ts) ~ gap 10 . pad 10 . flexRow $ do
    field "tag" ~ grow $ do
      label $ do
        input TextInput @ placeholder "New Tag" ~ border 1 . pad 10 @ value ""
    submit "+ Add" ~ btn
 where
  tagView (Tag t) = el ~ border 1 . pad 5 $ text t
