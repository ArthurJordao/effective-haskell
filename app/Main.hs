module Main (main) where

import System.Environment (getArgs)
import TraverseDirectory (directorySummaryWithMetrics)

main :: IO ()
main = getArgs >>= directorySummaryWithMetrics . head
