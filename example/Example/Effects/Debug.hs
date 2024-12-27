{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.Effects.Debug where

import Control.Concurrent (threadDelay)
import Data.String.Interpolate (i)
import Effectful
import Effectful.Dispatch.Dynamic

type Milliseconds = Int
data Debug :: Effect where
  Dump :: (Show a) => String -> a -> Debug m ()
  Delay :: Milliseconds -> Debug m ()

type instance DispatchOf Debug = 'Dynamic

runDebugIO
  :: (IOE :> es)
  => Eff (Debug : es) a
  -> Eff es a
runDebugIO = interpret $ \_ -> \case
  Dump msg a -> do
    liftIO $ putStrLn [i| [#{msg}] #{show a}|]
  Delay ms -> liftIO $ threadDelay (ms * 1000)

dump :: (Debug :> es, Show a) => String -> a -> Eff es ()
dump msg a = send $ Dump msg a

delay :: (Debug :> es) => Milliseconds -> Eff es ()
delay n = send $ Delay n
