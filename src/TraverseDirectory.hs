{-# LANGUAGE TypeApplications #-}

module TraverseDirectory
  ( traverseDirectory,
    traverseDirectory',
    longestContents,
    naiveTraversal,
    directorySummaryWithMetrics,
  )
where

import Control.Exception (IOException, handle)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef, modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set (Set, empty, insert, member)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Metrics
  ( Metrics,
    displayMetrics,
    newMetrics,
    tickFailure,
    tickSuccess,
    timeFunction,
  )
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import Text.Printf (printf)

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularType
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularType
    _ -> FileTypeOther

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther ->
      pure []
    FileTypeRegularType ->
      pure [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      concat <$> getPaths contents
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (`naiveTraversal` action)

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef @(Set.Set String) Set.empty
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef
      addDirectoryToSeen canonicalPath = do
        modifyIORef seenRef $ Set.insert canonicalPath
      handler ex = print ex >> tickFailure metrics
      traverseSubdirectory subdirPath =
        timeFunction metrics "traverseSubdirectory" $ do
          contents <- listDirectory subdirPath
          for_ contents $ \file' -> do
            handle @IOException handler $ do
              let file = subdirPath <> "/" <> file'
              canonicalPath <- canonicalizePath file
              classification <- classifyFile canonicalPath
              result <- case classification of
                FileTypeOther -> pure ()
                FileTypeRegularType ->
                  action file
                FileTypeDirectory -> do
                  alreadyProcessed <- haveSeenDirectory file
                  unless alreadyProcessed $ do
                    addDirectoryToSeen file
                    traverseSubdirectory file
              tickSuccess metrics
              pure result
  traverseSubdirectory (dropSuffix "/" rootPath)

traverseDirectory' :: Metrics -> FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' metrics rootPath action = do
  resultsRef <- newIORef []
  traverseDirectory metrics rootPath $ \file -> do
    modifyIORef resultsRef (action file :)
  readIORef resultsRef

longestContents :: Metrics -> FilePath -> IO ByteString
longestContents metrics rootPath = do
  contentsRef <- newIORef BS.empty
  let takeLongestFile a b = if BS.length a >= BS.length b then a else b
  traverseDirectory metrics rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef contentsRef (takeLongestFile contents)
  readIORef contentsRef

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- newMetrics
  histogramRef <- newIORef Map.empty
  traverseDirectory metrics root $ \file -> do
    putStrLn $ file <> ":"
    contents <-
      timeFunction metrics "TextIO.readFile" $
        TextIO.readFile file
    timeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
       in putStrLn $ "    word: count: " <> show wordCount
    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let addCharToHistogram histogram letter =
            Map.insertWith (+) letter (1 :: Int) histogram
          newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
      modifyIORef' histogramRef (const newHistogram)
  histogram <- readIORef histogramRef
  putStrLn "Histogram Data:"
  for_ (Map.toList histogram) $ \(letter, count) ->
    putStrLn $ printf "    %c: %d" letter count
  displayMetrics metrics
