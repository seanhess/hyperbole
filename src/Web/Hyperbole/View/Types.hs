{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.View.Types where

import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Static
import Web.Atomic.Html (Html (..))
import Web.Atomic.Html qualified as Atomic
import Web.Atomic.Types


-- View ------------------------------------------------------------

newtype View c a = View {html :: Eff '[Reader c] (Html a)}


instance IsString (View c ()) where
  fromString s = View $ pure $ Atomic.text (pack s)


runView :: forall c a. c -> View c a -> Html a
runView c (View eff) = do
  runPureEff $ runReader c eff


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
    hb <- eb
    pure $ hb


-- Context -----------------------------------------

type family ViewContext (v :: Type) where
  ViewContext (View c x) = c
  ViewContext (View c x -> View c x) = c


-- TEST: appending Empty
context :: forall c. View c c
context = View $ do
  c <- ask @c
  pure $ pure c


addContext :: ctx -> View ctx () -> View c ()
addContext c (View eff) = View $ do
  pure $ runPureEff $ runReader c eff


-- Html ---------------------------------------------

tag :: Text -> View c () -> View c ()
tag n (View eff) = View $ do
  content <- eff
  pure $ Atomic.tag n content


text :: Text -> View c ()
text t = View $ pure $ Atomic.text t


none :: View c ()
none = View $ pure Atomic.none


raw :: Text -> View c ()
raw t = View $ pure $ Atomic.raw t


-- Attributes -----------------------------------------

instance Attributable (View c a) where
  setAttribute n av (View eff) = View $ do
    h <- eff
    pure $ setAttribute n av h


instance Styleable (View c a) where
  modCSS f (View eff) = View $ do
    h <- eff
    pure $ modCSS f h
