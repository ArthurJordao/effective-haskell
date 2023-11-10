module Metrics
  ( AppMetrics,
    metrics,
    printMetrics,
  )
where

import Data.IORef
import qualified Data.Map.Strict as Map

data AppMetrics = AppMetrics
  { successCount :: Int,
    failureCount :: Int,
    callDuration :: Map.Map String Int
  }
  deriving (Eq, Show)

metrics :: IO (IORef AppMetrics)
metrics =
  newIORef
    AppMetrics
      { successCount = 0,
        failureCount = 0,
        callDuration = Map.empty
      }

printMetrics :: IO ()
printMetrics = metrics >>= readIORef >>= print
