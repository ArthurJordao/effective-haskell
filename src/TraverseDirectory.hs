{-# LANGUAGE TypeApplications #-}

module TraverseDirectory
  ( traverseDirectory,
    traverseDirectory',
    longestContents,
    naiveTraversal,
  )
where

import Control.Exception (IOException, handle)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isSuffixOf)
import qualified Data.Set as Set (Set, empty, insert, member)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )

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

traverseDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory rootPath action = do
  seenRef <- newIORef @(Set.Set String) Set.empty
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef
      addDirectoryToSeen canonicalPath = do
        modifyIORef seenRef $ Set.insert canonicalPath
      traverseSubdirectory subdirPath = do
        contents <- listDirectory subdirPath
        for_ contents $ \file' -> do
          handle @IOException (\_ -> pure ()) $ do
            let file = subdirPath <> "/" <> file'
            canonicalPath <- canonicalizePath file
            classification <- classifyFile canonicalPath
            case classification of
              FileTypeOther -> pure ()
              FileTypeRegularType ->
                action file
              -- modifyIORef resultRef (\results -> action file : results)
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDirectory file
                unless alreadyProcessed $ do
                  addDirectoryToSeen file
                  traverseSubdirectory file
  traverseSubdirectory (dropSuffix "/" rootPath)

traverseDirectory' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' rootPath action = do
  resultsRef <- newIORef []
  traverseDirectory rootPath $ \file -> do
    modifyIORef resultsRef (action file :)
  readIORef resultsRef

longestContents :: FilePath -> IO ByteString
longestContents rootPath = do
  contentsRef <- newIORef BS.empty
  let takeLongestFile a b = if BS.length a >= BS.length b then a else b
  traverseDirectory rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef contentsRef (takeLongestFile contents)
  readIORef contentsRef
