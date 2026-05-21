{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.Effects.Debug
  ( Milliseconds
  , Debug (..)
  , runDebugIO
  , dump
  , delay
  , systemTime
  , UTCTime
  ) where

import Control.Concurrent (threadDelay)
import Data.String.Interpolate (i)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic

type Milliseconds = Int
data Debug :: Effect where
  Dump :: (Show a) => String -> a -> Debug m ()
  Delay :: Milliseconds -> Debug m ()
  Time :: Debug m UTCTime

type instance DispatchOf Debug = 'Dynamic

runDebugIO
  :: (IOE :> es)
  => Eff (Debug : es) a
  -> Eff es a
runDebugIO = interpret $ \_ -> \case
  Dump msg a -> do
    liftIO $ putStrLn [i| [#{msg}] #{show a}|]
  Delay ms -> liftIO $ threadDelay (ms * 1000)
  Time -> liftIO getCurrentTime

dump :: (Debug :> es, Show a) => String -> a -> Eff es ()
dump msg a = send $ Dump msg a

delay :: (Debug :> es) => Milliseconds -> Eff es ()
delay n = send $ Delay n

systemTime :: (Debug :> es) => Eff es UTCTime
systemTime = send Time
