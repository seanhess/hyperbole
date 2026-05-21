module Example.Concurrency.Tasks where

import Data.Text (Text, pack)
import Effectful
import Example.Effects.Debug
import Web.Hyperbole.Effect.GenRandom

-- Fake Tasks Effect ----------------------------------------

type TaskId = Int

data Task = Task
  { taskId :: TaskId
  , details :: Text
  }

pretendLoadTask :: (Debug :> es, GenRandom :> es) => TaskId -> Eff es Task
pretendLoadTask taskId = do
  randomDelay <- genRandom (100, 1000)
  delay randomDelay

  pure $ Task taskId $ pack (show taskId)

pretendTasks :: [TaskId]
pretendTasks = [1 .. 30]
