module Metrics
  ( AppMetrics,
    newMetrics,
    printMetrics,
    Metrics (..),
    timeFunction,
    tickSuccess,
    tickFailure,
    displayMetrics,
  )
where

import Data.Foldable (for_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Text.Printf (printf)

data AppMetrics = AppMetrics
  { successCount :: Int,
    failureCount :: Int,
    callDuration :: Map.Map String Int
  }
  deriving (Eq, Show)

newtype Metrics = Metrics {appMetricsStore :: IORef AppMetrics}

metrics :: IO (IORef AppMetrics)
metrics =
  newIORef
    AppMetrics
      { successCount = 0,
        failureCount = 0,
        callDuration = Map.empty
      }

newMetrics :: IO Metrics
newMetrics = Metrics <$> metrics

printMetrics :: IO ()
printMetrics = metrics >>= readIORef >>= print

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) =
  modifyIORef metricsRef $ \m ->
    m {successCount = 1 + m.successCount}

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
  m {failureCount = 1 + m.failureCount}

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metricsRef) actionName action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  modifyIORef metricsRef $ \oldMetrics ->
    let oldDurationValue =
          fromMaybe 0 $ Map.lookup actionName oldMetrics.callDuration
        runDuration =
          floor . nominalDiffTimeToSeconds $
            diffUTCTime endTime startTime
        newDurationValue = oldDurationValue + runDuration
     in oldMetrics
          { callDuration =
              Map.insert
                actionName
                newDurationValue
                oldMetrics.callDuration
          }
  pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  appMetrics <- readIORef metricsStore
  putStrLn $ "successes:" <> show appMetrics.successCount
  putStrLn $ "failures:" <> show appMetrics.failureCount
  for_ (Map.toList appMetrics.callDuration) $ \(functionName, timing) ->
    putStrLn $ printf "Time spent in \"%s\": %d" functionName timing
