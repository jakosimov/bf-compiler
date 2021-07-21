module Lib
    ( someFunc
    ) where

import System.IO

testFile :: String
testFile = "test.c"

doThing :: String -> String
doThing = undefined

someFunc :: IO ()
someFunc =
  do handle <- openFile testFile ReadMode
     contents <- hGetContents handle
     let result = doThing contents
     print result
     hClose handle
