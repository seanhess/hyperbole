{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.View.Element where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text)
import Effectful
import Effectful.Writer.Static.Local
import Web.View.Style
import Web.View.Types
import Web.View.Types.Url
import Web.View.View


{- | A basic element

> el (bold . pad 10) "Hello"
-}
el :: Mod c -> View c () -> View c ()
el = tag "div"


{- | A basic element, with no modifiers

> el_ "Hello"
-}
el_ :: View c () -> View c ()
el_ = tag "div" id


{- | Add text to a view. Not required for string literals

> el_ $ do
>   "Hello: "
>   text user.name
-}
text :: Text -> View c ()
text t = viewAddContent $ Text t


{- | Embed static, unescaped HTML or SVG. Take care not to use 'raw' with user-generated content.

> spinner = raw "<svg>...</svg>"
-}
raw :: Text -> View c ()
raw t = viewAddContent $ Raw t


{- | Do not show any content

> if isVisible
>  then content
>  else none
-}
none :: View c ()
none = pure ()


pre :: Mod c -> Text -> View c ()
pre f t = tag "pre" f (text t)


code :: Mod c -> Text -> View c ()
code f t = tag "code" f (text t)


-- | A hyperlink to the given url
link :: Url -> Mod c -> View c () -> View c ()
link u f = tag "a" (att "href" (renderUrl u) . f)


-- * Inputs


form :: Mod c -> View c () -> View c ()
form = tag "form"


input :: Mod c -> View c ()
input m = tag "input" (m . att "type" "text") none


name :: Text -> Mod c
name = att "name"


value :: Text -> Mod c
value = att "value"


label :: Mod c -> View c () -> View c ()
label = tag "label"


button :: Mod c -> View c () -> View c ()
button = tag "button"


-- * Document Metadata


script :: Text -> View c ()
script src = tag "script" (att "type" "text/javascript" . att "src" src) none


style :: Text -> View c ()
style cnt = tag "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")


stylesheet :: Text -> View c ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none


-- * Tables


{- | Create a type safe data table by specifying columns

> usersTable :: [User] -> View c ()
> usersTable us = do
>   table id us $ do
>     tcol (th hd "Name") $ \u -> td cell $ text u.name
>     tcol (th hd "Email") $ \u -> td cell $ text u.email
>  where
>   hd = cell . bold
>   cell = pad 4 . border 1
-}
table :: Mod c -> [dt] -> Eff '[Writer [TableColumn c dt]] () -> View c ()
table f dts wcs = do
  c <- context
  let cols = runPureEff . execWriter $ wcs
  tag "table" borderCollapse $ do
    tag "thead" id $ do
      tag "tr" f $ do
        forM_ cols $ \tc -> do
          addContext (TableHead c) tc.headCell
    tag "tbody" id $ do
      forM_ dts $ \dt -> do
        tag "tr" f $ do
          forM_ cols $ \tc -> do
            addContext dt $ tc.dataCell dt
 where
  borderCollapse :: Mod c
  borderCollapse = addClass $ cls "brd-cl" & prop @Text "border-collapse" "collapse"


tcol :: forall dt c. View (TableHead c) () -> (dt -> View dt ()) -> Eff '[Writer [TableColumn c dt]] ()
tcol hd view = do
  tell ([TableColumn hd view] :: [TableColumn c dt])


th :: Mod c -> View c () -> View (TableHead c) ()
th f cnt = do
  TableHead c <- context
  addContext c $ tag "th" f cnt


td :: Mod () -> View () () -> View dt ()
td f c = addContext () $ tag "td" f c


newtype TableHead a = TableHead a


data TableColumn c dt = TableColumn
  { headCell :: View (TableHead c) ()
  , dataCell :: dt -> View dt ()
  }


-- * Lists


newtype ListItem c a = ListItem (View c a)
  deriving newtype (Functor, Applicative, Monad)


{- | List elements do not include any inherent styling but are useful for accessibility. See 'Web.View.Style.list'.

> ol id $ do
>  let nums = list Decimal
>  li nums "one"
>  li nums "two"
>  li nums "three"
-}
ol :: Mod c -> ListItem c () -> View c ()
ol f (ListItem cnt) = do
  tag "ol" f cnt


ul :: Mod c -> ListItem c () -> View c ()
ul f (ListItem cnt) = do
  tag "ul" f cnt


li :: Mod c -> View c () -> ListItem c ()
li f cnt = ListItem $ do
  tag "li" f cnt
