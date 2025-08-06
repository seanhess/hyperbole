{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.GenRandom where

import Control.Monad (replicateM)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random (Random, randomRIO)


data GenRandom :: Effect where
  GenRandom :: (Random a) => (a, a) -> GenRandom m a
  GenRandomToken :: Int -> GenRandom m Text
  GenRandomList :: (Random a) => [a] -> GenRandom m a


type instance DispatchOf GenRandom = 'Dynamic


runRandom
  :: (IOE :> es)
  => Eff (GenRandom : es) a
  -> Eff es a
runRandom = interpret $ \_ -> \case
  GenRandom range -> liftIO $ randomRIO range
  GenRandomToken n -> do
    let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    randStr <- liftIO $ replicateM n (randomFromList chars)
    pure $ cs randStr
  GenRandomList as ->
    liftIO $ randomFromList as
 where
  randomFromList :: (Random a) => [a] -> IO a
  randomFromList as = do
    index <- liftIO $ randomRIO (0, length as - 1)
    pure $ as !! index


genRandom :: (Random a, GenRandom :> es) => (a, a) -> Eff es a
genRandom range = send $ GenRandom range


genRandomToken :: (GenRandom :> es) => Int -> Eff es Text
genRandomToken num = send $ GenRandomToken num


genRandomList :: (Random a, GenRandom :> es) => [a] -> Eff es a
genRandomList as = send $ GenRandomList as
