{-# LANGUAGE TypeApplications #-}

module Pager
  ( pager,
  )
where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.IO.Error as IOError

pager :: IO ()
pager =
  handleIOError $
    handleArgs
      >>= eitherToError
      >>= TextIO.readFile
      >>= TextIO.putStr
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction =
      Exception.catch ioAction (\e -> putStr "I ran into an error: " >> print @IOError e)

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [arg] -> Right arg
        [] -> Left "no arguments provided"
        _ -> Left "error multiple files not supported"

eitherToError :: (Show a) => Either a b -> IO b
eitherToError (Right a) = return a
eitherToError (Left e) = Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf 0 _ = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
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
