{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.View.Types where

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import GHC.Generics
import Web.Atomic.Html (Html (..))
import Web.Atomic.Html qualified as Atomic
import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded (decodeEither, encodedToText)
import Web.Hyperbole.Data.Param (FromParam, ToParam (..))
import Web.Hyperbole.View.ViewId


-- View ------------------------------------------------------------

{- | 'View's are HTML fragments with a 'context'

@
#EMBED Example.Docs.BasicPage helloWorld
@
-}
newtype View c a = View {html :: Eff '[Reader (c, ViewState c)] (Html a)}


instance IsString (View c ()) where
  fromString s = View $ pure $ Atomic.text (pack s)


execView :: forall c a. c -> ViewState c -> View c a -> Html a
execView c st (View eff) = do
  runPureEff $ runReader (c, st) eff


instance Functor (View c) where
  fmap f (View eff) = View $ do
    html <- eff
    pure $ fmap f html
instance Applicative (View ctx) where
  pure a = View $ pure $ pure a
  liftA2 :: (a -> b -> c) -> View ctx a -> View ctx b -> View ctx c
  liftA2 abc (View va) (View vb) = View $ do
    ha <- va
    hb <- vb
    pure $ liftA2 abc ha hb
  View va *> View vb = View $ do
    ha <- va
    hb <- vb
    pure $ ha *> hb
instance Monad (View ctx) where
  (>>) = (*>)
  (>>=) :: forall a b. View ctx a -> (a -> View ctx b) -> View ctx b
  -- TEST: appending Empty
  View ea >>= famb = View $ do
    a :: a <- (.value) <$> ea
    let View eb :: View ctx b = famb a
    eb


-- Context -----------------------------------------

-- type family ViewContext (v :: Type) where
--   ViewContext (View c x) = c
--   ViewContext (View c x -> View c x) = c

newtype ChildView a = ChildView a
  deriving (Generic)
instance (ViewId a, FromParam a, ToParam a) => ViewId (ChildView a) where
  type ViewState (ChildView a) = ViewState a


-- TEST: appending Empty
context :: forall c. View c (c, ViewState c)
context = View $ do
  c <- ask @(c, ViewState c)
  pure $ pure c


viewState :: View c (ViewState c)
viewState = snd <$> context


runViewContext :: ctx -> ViewState ctx -> View ctx () -> View c ()
runViewContext c st (View eff) = View $ do
  pure $ runPureEff $ runReader (c, st) eff


runChildView :: (ViewState ctx ~ ViewState c) => (c -> ctx) -> View ctx () -> View c ()
runChildView f v = do
  st <- viewState
  c <- viewId
  runViewContext (f c) st v


-- modifyContext
--   :: forall ctx0 ctx1. (ctx0 -> ctx1) -> View ctx1 () -> View ctx0 ()
-- modifyContext f (View eff) = View $ do
--   ctx0 <- ask @ctx0
--   pure $ runPureEff $ runReader (f ctx0) eff

-- Attributes -----------------------------------------

instance Attributable (View c a) where
  modAttributes f (View eff) = View $ do
    h <- eff
    pure $ modAttributes f h


instance Styleable (View c a) where
  modCSS f (View eff) = View $ do
    h <- eff
    pure $ modCSS f h


{- | Access the 'viewId' in a 'View' or 'update'

@
#EMBED Example.Concurrency.LazyLoading data LazyData

#EMBED Example.Concurrency.LazyLoading instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
@
-}
class HasViewId m view where
  viewId :: m view


instance HasViewId (View ctx) ctx where
  viewId = fst <$> context
instance (ViewState view ~ st) => HasViewId (Eff (Reader view : State st : es)) view where
  viewId = ask


encodeViewId :: (ViewId id) => id -> Text
encodeViewId = encodedToText . toViewId


decodeViewId :: (ViewId id) => Text -> Maybe id
decodeViewId t = do
  case parseViewId =<< decodeEither t of
    Left _ -> Nothing
    Right a -> pure a
