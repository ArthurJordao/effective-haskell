{-# LANGUAGE TypeApplications #-}

module TraverseDirectory where

import Control.Exception (IOException, handle)
import Control.Monad (join, unless, void)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Set as Set (Set, empty, insert, member)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
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

traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
  seenRef <- newIORef @(Set.Set String) Set.empty
  resultRef <- newIORef []
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
                modifyIORef resultRef (\results -> action file : results)
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDirectory file
                unless alreadyProcessed $ do
                  addDirectoryToSeen file
                  traverseSubdirectory file
  traverseSubdirectory (dropSuffix "/" rootPath)
  readIORef resultRef
