module Pager (pager) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf (printf)

data Command
  = Continue
  | Cancel
  deriving (Eq, Show)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWriteable :: Bool,
    fileExecutable :: Bool,
    contents :: Text
  }
  deriving (Show)

pager :: IO ()
pager = do
  paths <- eitherToError =<< handleArgs
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfos <- traverse fileInfo paths
  let pages = finfos >>= paginate termSize
  showPages pages

handleArgs :: IO (Either Text [FilePath])
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [] -> Left "no arguments provided"
        arg -> Right arg

eitherToError :: (Show a) => Either a b -> IO b
eitherToError (Right a) = return a
eitherToError (Left e) = Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf 0 _ = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

wordWrap :: Int -> Text -> [Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overFlow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overFlow <> nextLines)
  where
    softWrap handwrappedText textIndex
      | textIndex <= 0 = (handwrappedText, Text.empty)
      | Text.index handwrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex handwrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap handwrappedText (textIndex - 1)

paginate :: ScreenDimensions -> FileInfo -> [Text]
paginate dimensions finfo =
  let rows' = dimensions.screenRows - 1
      wrappedLines = concatMap (wordWrap dimensions.screenColumns) (Text.lines finfo.contents)
      pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo dimensions.screenColumns pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> handleIOError tputScreenDimensions
    "linux" -> handleIOError tputScreenDimensions
    _ -> pure $ ScreenDimensions {screenRows = 25, screenColumns = 80}
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      rows <- readProcess "tput" ["lines"] ""
      cols <- readProcess "tput" ["cols"] ""
      return $
        ScreenDimensions
          { screenRows = read $ init rows,
            screenColumns = read $ init cols
          }
    handleIOError :: IO a -> IO a
    handleIOError ioAction =
      Exception.catch ioAction $
        \(_ :: IOError) -> Exception.throw . IOError.userError $ "tput not found"

getCommand :: IO Command
getCommand =
  hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> getChar
    >>= \case
      ' ' -> return Continue
      'q' -> return Cancel
      _ -> getCommand

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

showPages :: [Text] -> IO ()
showPages [] = return ()
showPages (page : pages) =
  clearScreen
    >> TextIO.putStr page
    >> getCommand
    >>= \case
      Continue -> showPages pages
      Cancel -> return ()

fileInfo :: FilePath -> IO FileInfo
fileInfo path = do
  contents <- TextIO.hGetContents =<< openFile path ReadMode
  perms <- Directory.getPermissions path
  mtime <- Directory.getModificationTime path
  size <- BS.length <$> BS.readFile path
  return
    FileInfo
      { filePath = path,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWriteable = Directory.writable perms,
        fileExecutable = Directory.executable perms,
        contents = contents
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text
formatFileInfo file maxWidth totalPages currentPage =
  let permissionString =
        [ if file.fileReadable then 'r' else '-',
          if file.fileWriteable then 'w' else '-',
          if file.fileExecutable then 'x' else '-'
        ]
      timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" file.fileMTime
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            file.filePath
            permissionString
            file.fileSize
            timestamp
            currentPage
            totalPages
   in invertText $ truncateStatus statusLine
  where
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine
    invertText inputStr =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> inputStr <> resetVideo
