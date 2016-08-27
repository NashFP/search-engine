module Main where

import System.IO
import Data.ByteString.Char8 (pack)
import Data.Map
import Lib

readLines index = do
  putStr "Search term: "
  hFlush stdout
  line <- getLine
  putStrLn $ show (take 10 (search (pack line) index))
  readLines index

main :: IO ()
main = do
  wordsIndex <- loadIndex
  putStrLn ("Loaded index with "++(show $ (size wordsIndex))++" entries")
  readLines wordsIndex

