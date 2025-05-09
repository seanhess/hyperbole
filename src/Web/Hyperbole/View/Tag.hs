{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.View.Tag where

import Control.Monad (forM_)
import Data.Text
import Effectful
import Effectful.Writer.Static.Local
import Web.Atomic.CSS
import Web.Atomic.Types
import Web.Hyperbole.Data.URI
import Web.Hyperbole.View.Types


el :: View c () -> View c ()
el = tag "div"


row :: View c () -> View c ()
row = tag "div" ~ flexRow


col :: View c () -> View c ()
col = tag "div" ~ flexCol


space :: View c ()
space = tag "div" none ~ grow


pre :: Text -> View c ()
pre t = tag "pre" (text t)


code :: Text -> View c ()
code t = tag "code" (text t)


-- | A hyperlink to the given url
link :: URI -> View c () -> View c ()
link u = tag "a" @ att "href" (uriToText u)


img :: Text -> View c ()
img sc = tag "img" @ src sc $ none


-- * Inputs


-- basic forms. See Web.Hyperbole.View.Forms
form :: View c () -> View c ()
form = tag "form"


input :: View c ()
input = tag "input" @ att "type" "text" $ none


name :: (Attributable h) => Text -> Attributes h -> Attributes h
name = att "name"


value :: (Attributable h) => Text -> Attributes h -> Attributes h
value = att "value"


label :: View c () -> View c ()
label = tag "label"


placeholder :: (Attributable h) => Text -> Attributes h -> Attributes h
placeholder = att "placeholder"


autofocus :: (Attributable h) => Attributes h -> Attributes h
autofocus = att "autofocus" ""


-- * Document Metadata


type_ :: (Attributable h) => Text -> Attributes h -> Attributes h
type_ = att "type"


src :: (Attributable h) => Text -> Attributes h -> Attributes h
src = att "src"


script :: Text -> View c ()
script s = tag "script" none @ type_ "text/javascript" @ src s


style :: Text -> View c ()
style cnt = tag "style" (text $ "\n" <> cnt <> "\n") @ type_ "text/css"


stylesheet :: Text -> View c ()
stylesheet href = tag "link" @ att "rel" "stylesheet" . att "href" href $ none


-- * Navigation


nav :: View c () -> View c ()
nav = tag "nav"


-- * Tables


{- | Create a type safe data table by specifying columns

> data User = User {name :: Text, email :: Text}
>
> usersTable :: [User] -> View c ()
> usersTable us = do
>   table us $ do
>     tcol (th "Name" ~ hd) $ \u -> td ~ cell $ text u.name
>     tcol (th "Email" ~ hd) $ \u -> td ~ cell $ text u.email
>  where
>   hd = cell . bold
>   cell :: (Styleable h) => CSS h -> CSS h
>   cell = pad 4 . border 1
-}
table :: [dt] -> Eff '[Writer [TableColumn c dt]] () -> View c ()
table dts wcs = do
  let cols = runPureEff . execWriter $ wcs
  tag "table" ~ borderCollapse $ do
    tag "thead" $ do
      tag "tr" $ do
        forM_ cols $ \tc -> do
          let TableHead hd = tc.headCell
          hd
    tag "tbody" $ do
      forM_ dts $ \dt -> do
        tag "tr" $ do
          forM_ cols $ \tc -> do
            tc.dataCell dt
 where
  borderCollapse = utility @Text "brd-cl" "border-collapse" "collapse"


tcol :: forall dt c. TableHead c () -> (dt -> View c ()) -> Eff '[Writer [TableColumn c dt]] ()
tcol hd cell = do
  tell ([TableColumn hd cell] :: [TableColumn c dt])


th :: View c () -> TableHead c ()
th cnt = do
  TableHead $ tag "th" cnt


td :: View c () -> View c ()
td = tag "td"


instance {-# OVERLAPS #-} Styleable (Eff '[Writer [TableColumn c dt]] () -> View c ()) where
  modCSS frr parent eff = modCSS frr (parent eff)


newtype TableHead c a = TableHead (View c a)
  deriving newtype (Functor, Applicative, Monad, Styleable)


data TableColumn c dt = TableColumn
  { headCell :: TableHead c ()
  , dataCell :: dt -> View c ()
  }


-- * Lists


{- | List elements do not include any inherent styling but are useful for accessibility. See 'Web.Atomic.CSS.list'.

> ol id $ do
>  let nums = list Decimal
>  li nums "one"
>  li nums "two"
>  li nums "three"
-}
ol :: ListItem c () -> View c ()
ol (ListItem cnt) = do
  tag "ol" cnt


ul :: ListItem c () -> View c ()
ul (ListItem cnt) = do
  tag "ul" cnt


li :: View c () -> ListItem c ()
li cnt = ListItem $ do
  tag "li" cnt


newtype ListItem c a = ListItem (View c a)
  deriving newtype (Functor, Applicative, Monad, Styleable)
