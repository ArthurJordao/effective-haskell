module IOActions
  ( doSomeFileStuff,
    ioIOString,
    ioIOToIO,
    myTraverse,
    calculator,
  )
where

import Control.Monad (join)
import Data.Text (pack, replace, unpack)
import System.IO (IOMode (ReadMode, ReadWriteMode), hClose, hGetContents, hPutStr, openFile)

doSomeFileStuff :: IO ()
doSomeFileStuff =
  openFile "/tmp/foo.txt" ReadMode
    >>= \handle ->
      hGetContents handle
        >>= putStrLn
        >> hClose handle

-- “Write a function that returns a value of type IO (IO String).  What
-- happens if you try to use >>= with that?  What if you want to print
-- the string?”
ioIOString :: IO (IO String)
ioIOString = return (return "hello")

-- “Using your function from the previous example, create a function that
-- has the type signature: IO (IO a) -> IO a.”

ioIOToIO :: IO (IO a) -> IO a
ioIOToIO = join

-- “Write a function that returns a value of type [IO a], and a second
-- function with the type [IO a] -> IO [a].  When might you use a
-- function like that?”

myTraverse :: [IO a] -> IO [a]
myTraverse =
  foldl
    ( \listOfIOs currentIO -> do
        curList <- listOfIOs
        currentValue <- currentIO
        return (curList <> [currentValue])
    )
    (return [] :: IO [a])

-- “Write a program that reads in numbers from the command line and prints the
-- sum of the provided values.
--
-- Modify your previous program so that the first argument is an operation (+,
-- -, or *) and performs the supplied operation on the list of numbers.”

calculator :: [Integer] -> IO ()
calculator list = do
  currentLine <- getLine
  if currentLine == ""
    then print (sum list)
    else do
      calculator ((read currentLine :: Integer) : list)

findAndReplaceInFile :: String -> String -> String -> IO ()
findAndReplaceInFile filePath newWord oldWord = do
  contents <- readFile filePath
  let contentReplaced = replace (pack oldWord) (pack newWord) (pack contents)
  writeFile filePath (unpack contentReplaced)
  return ()
