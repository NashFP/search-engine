module Lib where

import qualified Data.Map.Strict as Map
import Data.ByteString.Char8 (ByteString, splitWith, unpack, pack, length)
import Data.List
import Data.List.Split
import Data.Char
import Control.Concurrent.Async
import Control.Concurrent.ParallelIO.Local
import Control.Monad
import Control.Applicative
import Control.DeepSeq
import System.Directory
import qualified System.IO.Strict as StrictIO

type IndexEntry = (ByteString,Int)
type WordIndex = Map.Map ByteString [IndexEntry]
type FileIndex = Map.Map ByteString Int

-- Looks up a term in the word index and returns all the entries sorted from most- to
-- least- frequent
search :: ByteString -> WordIndex -> [IndexEntry]
search term index =
  sortOn sortFunc findItem
    where
      findItem = Map.findWithDefault [] term index
      sortFunc (f,i) = -i

-- Splits a string into an array of words, using any non-letter as a separator
tokenize :: ByteString -> [ByteString]
tokenize str =
  filter (\s -> Data.ByteString.Char8.length s > 0) $ splitWith (not . isLetter) str

-- Loads a file and creates a map counting the frequency of each word
loadFile :: String -> IO (ByteString,FileIndex)
loadFile filename = do
  text <- StrictIO.readFile filename
  let title = pack $ head (lines text)
  let words = tokenize $ pack $ map toLower text
  let wordMap = foldl' addWord Map.empty words
  return (title,wordMap)
      where
        addWord wordMap word = Map.insertWith (+) word 1 wordMap

-- Loads a batch of files in parallel
loadIndexBatch :: [String] -> WordIndex -> IO WordIndex
loadIndexBatch files currMap = do
  tasks <- mapM async (map loadFile files)
  indexes <- mapM wait tasks
  return (createIndex currMap indexes)

-- Loads the next batch of files and uses deepseq to make sure the index isn't lazy
loadBatches :: [[String]] -> WordIndex -> IO WordIndex
loadBatches [] currMap = do
  return currMap

loadBatches (files:rest) currMap = do
  nextMap <- loadIndexBatch files currMap
  nextMap `deepseq` loadBatches rest nextMap

-- Loads all the files in ../sample
loadIndex :: IO WordIndex
loadIndex = do
  dirContents <- getDirectoryContents "../sample"
  files <- filterM doesFileExist $ map (\f -> "../sample/"++f) dirContents
  loadBatches (chunksOf 1000 files) Map.empty

-- Adds a file index to the word index. The file index is just words and counts, like:
-- { "foo": 5, "bar": 6, "baz": 3, "quux": 2}
-- The word index is keyed by word, and the entries are title,count pairs, like:
-- { "foo": [("Metavariables": 5, "Adventure": 3)], "baz": [("Metavariables": 3)] }
addToIndex :: WordIndex -> (ByteString, FileIndex) -> WordIndex
addToIndex index (title,wordMap) =
  foldl' addItem index (Map.toList wordMap)
    where
      addItem currIndex (word,count) = Map.insertWith (++) word [(title,count)] currIndex
-- Takes a list of file indexes and adds them to an existing word index
createIndex :: WordIndex -> [(ByteString, FileIndex)] -> WordIndex
createIndex startMap wordMaps =
  foldl' addToIndex startMap wordMaps


