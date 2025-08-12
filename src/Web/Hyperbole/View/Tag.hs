{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.View.Tag where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import Effectful.State.Static.Local
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


-- * Header


title :: Text -> View c ()
title = tag "title" . text


meta :: View c ()
meta = tag "meta" none


httpEquiv :: (Attributable c) => Text -> Attributes c -> Attributes c
httpEquiv = att "httpEquiv"


content :: (Attributable c) => Text -> Attributes c -> Attributes c
content = att "content"


charset :: (Attributable c) => Text -> Attributes c -> Attributes c
charset = att "charset"


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
script s = tag "script" @ type_ "text/javascript" . src s $ none


script' :: ByteString -> View c ()
script' cdata = tag "script" @ type_ "text/javascript" $ raw $ cs cdata


style :: ByteString -> View c ()
style cnt = tag "style" (raw $ "\n" <> cs cnt <> "\n") @ type_ "text/css"


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
table :: [dt] -> TableColumns c dt () -> View c ()
table dts (TableColumns wcs) = do
  let cols = runPureEff . execState [] $ wcs
  tag "table" $ do
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


usersTable :: View c ()
usersTable = do
  table items $ do
    tcol (th "Index" ~ bold) $ \u -> td ~ cell $ text $ pack $ show $ fst u
    tcol (th "Item" ~ bold) $ \u -> td ~ cell $ text $ snd u
 where
  items :: [(Int, Text)]
  items = zip [0 ..] ["one", "two", "three"]
  cell :: (Styleable h) => CSS h -> CSS h
  cell = pad 4 . border 1


newtype Table c a = Table (View c a)
  deriving newtype (Functor, Applicative, Monad, Styleable)


tcol :: forall dt c. TableHead c () -> (dt -> View c ()) -> TableColumns c dt ()
tcol hd cell = TableColumns $ do
  modify @[TableColumn c dt] $ \cols -> cols <> [TableColumn hd cell]


th :: View c () -> TableHead c ()
th cnt = do
  TableHead $ tag "th" cnt


td :: View c () -> View c ()
td = tag "td"


instance {-# OVERLAPS #-} Styleable (TableColumns c dt () -> View c ()) where
  modCSS frr parent eff = modCSS frr (parent eff)


newtype TableHead id a = TableHead (View id a)
  deriving newtype (Functor, Applicative, Monad, Styleable)


newtype TableColumns c dt a = TableColumns (Eff '[State [TableColumn c dt]] a)
  deriving newtype (Functor, Applicative, Monad)


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
