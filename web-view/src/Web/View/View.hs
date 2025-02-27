{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

module Web.View.View where

import Data.Map.Strict qualified as M
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local as ES
import Web.View.Types


-- * Views


{- | Views are HTML fragments that carry all 'CSS' used by any child element.

> view :: View c ()
> view = col (pad 10 . gap 10) $ do
>   el bold "Hello"
>   el_ "World"

They can also have a context which can be used to create type-safe or context-aware elements. See 'context' or 'Web.View.Element.table' for an example
-}
newtype View context a = View {viewState :: Eff [Reader context, State ViewState] a}
  deriving newtype (Functor, Applicative, Monad)


instance IsString (View context ()) where
  fromString s = viewAddContent $ Text (pack s)


data ViewState = ViewState
  { contents :: [Content]
  , css :: CSS
  }


instance Semigroup ViewState where
  va <> vb = ViewState (va.contents <> vb.contents) (va.css <> vb.css)


-- | Extract the 'ViewState' from a 'View'
runView :: context -> View context () -> ViewState
runView ctx (View ef) =
  runPureEff . execState (ViewState [] []) . runReader ctx $ ef


{- | Views have a `Reader` built-in for convienient access to static data, and to add type-safety to view functions. See 'Web.View.Element.ListItem and https://hackage.haskell.org/package/hyperbole/docs/Web-Hyperbole.html

> numberView :: View Int ()
> numberView = do
>   num <- context
>   el_ $ do
>     "Number: "
>     text (pack $ show num)
-}
context :: View context context
context = View ask


{- | Run a view with a specific `context` in a parent 'View' with a different context.

>
> parentView :: View c ()
> parentView = do
>   addContext 1 numberView
>   addContext 2 numberView
>   addContext 3 numberView
-}
addContext :: context -> View context () -> View c ()
addContext ctx vw = do
  -- runs the sub-view in a different context, saving its state
  -- we need to MERGE it
  let st = runView ctx vw
  View $ do
    s <- get
    put $ s <> st


viewModContents :: ([Content] -> [Content]) -> View context ()
viewModContents f = View $ do
  ES.modify $ \s -> s{contents = f s.contents}


viewModCss :: (CSS -> CSS) -> View context ()
viewModCss f = View $ do
  ES.modify $ \s -> s{css = f s.css}


viewAddClasses :: CSS -> View c ()
viewAddClasses clss = do
  viewModCss $ \cm -> foldr addClsDef cm clss
 where
  addClsDef :: Class -> CSS -> CSS
  addClsDef c = M.insert c.selector c


viewAddContent :: Content -> View c ()
viewAddContent ct =
  viewModContents (<> [ct])


-- | Inserts contents into the first child element
viewInsertContents :: [Content] -> View c ()
viewInsertContents cs = viewModContents insert
 where
  insert [Node e] = [Node $ insertEl e]
  insert cnt = cnt <> cs
  insertEl e = e{children = e.children <> cs}


-- * Creating new Elements


{- | Create a new element constructor with the given tag name

> aside :: Mod c -> View c () -> View c ()
> aside = tag "aside"
-}
tag :: Text -> Mod c -> View c () -> View c ()
tag n = tag' (element n)


{- | Create a new element constructor with a custom element
 -
> span :: Mod c -> View c () -> View c ()
> span = tag' (Element True) "span"
-}
tag' :: (Attributes c -> [Content] -> Element) -> Mod c -> View c () -> View c ()
tag' mkElem f ct = do
  -- Applies the modifier and merges children into parent
  ctx <- context
  let st = runView ctx ct
  let ats = f mempty
  let elm = mkElem ats st.contents
  viewAddContent $ Node elm
  viewAddClasses st.css
  viewAddClasses elm.attributes.classes


{- | Set an attribute, replacing existing value

> hlink :: Text -> View c () -> View c ()
> hlink url content = tag "a" (att "href" url) content
-}
att :: Name -> AttValue -> Mod c
att n v attributes =
  let atts = M.insert n v attributes.other
   in attributes{other = atts}
