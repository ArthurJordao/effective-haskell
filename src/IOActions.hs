module IOActions
  ( doSomeFileStuff,
  )
where

import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

doSomeFileStuff :: IO ()
doSomeFileStuff =
  openFile "/tmp/foo.txt" ReadMode
    >>= \handle ->
      hGetContents handle
        >>= putStrLn
        >> hClose handle
