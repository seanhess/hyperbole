{-# LANGUAGE LambdaCase #-}

module Example.Effects.Random
  ( GenRandom
  , genRandom
  , runRandom
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (Random, randomRIO)

data GenRandom :: Effect where
  GenRandom :: (Random a) => (a, a) -> GenRandom m a

type instance DispatchOf GenRandom = 'Dynamic

runRandom
  :: (IOE :> es)
  => Eff (GenRandom : es) a
  -> Eff es a
runRandom = interpret $ \_ -> \case
  GenRandom range -> liftIO $ randomRIO range

genRandom :: (Random a, GenRandom :> es) => (a, a) -> Eff es a
genRandom range = send $ GenRandom range
