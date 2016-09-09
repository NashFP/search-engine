module Main where

import System.Environment
import Lib
import Data.Text
import qualified Data.Map as Map
import System.IO
import qualified Data.List as List

readLines :: WordMap -> IO ()
readLines index = do
  putStr "Search term: "
  hFlush stdout
  line <- getLine
  putStrLn $ show (List.take 10 (search (pack line) index))
  readLines index
  
main :: IO ()
main = do
  wiki <- load_wikipedia
  putStrLn ("Loaded wikipedia with "++(show $ Map.size wiki)++" entries")
  readLines wiki
  
