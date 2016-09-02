module Lib where

import qualified Data.Map.Strict as Map
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import Data.Char
import Control.Concurrent.Async
import Control.Monad
import System.Directory

data EntryType = EntryEmpty | EntryFile ByteString Int | EntryNode EntryType EntryType
type WordIndex = Map.Map ByteString EntryType
type FileIndex = Map.Map ByteString Int

-- Converts a tree of EntryTypes to a list
entryNodeToList :: EntryType -> [(ByteString,Int)]
entryNodeToList EntryEmpty = []
entryNodeToList (EntryFile bs c) = [(bs,c)]
entryNodeToList (EntryNode left right) =
  (entryNodeToList left) ++ (entryNodeToList right)

-- Looks up a term in the word index and returns all the entries sorted from most- to
-- least- frequent
search :: ByteString -> WordIndex -> [(ByteString,Int)]
search term index =
  List.sortOn sortFunc (entryNodeToList findItem)
    where
      findItem = Map.findWithDefault EntryEmpty term index
      sortFunc (f,i) = -i

-- Splits a string into an array of words, using any non-letter as a separator
tokenize :: ByteString -> [ByteString]
tokenize str =
  List.filter (\bs -> (BS.length bs) > 0) $ splitWith (not . isLetter) str

-- Loads a file and creates a map counting the frequency of each word
loadFile :: String -> IO WordIndex
loadFile filename = do
  text <- BS.readFile filename
  let title = BS.takeWhile (\c -> (c /= '\n') && (c /= '\r')) text
  let words = tokenize $ BS.map toLower text
  let tempWordMap = List.foldl' addWord Map.empty words
  return $ Map.map (addTitle title) tempWordMap
      where
        addWord wordMap word = Map.insertWith (\n o -> n `seq` o `seq` n+o) word 1 wordMap
        addTitle title c = title `seq` c `seq` EntryFile title c

-- Adds two EntryType trees together
mergeEntries :: EntryType -> EntryType -> EntryType
mergeEntries left right =
  left `seq` right `seq` EntryNode left right

-- Loads all the files in ../sample
loadIndex :: IO WordIndex
loadIndex = do
  dirContents <- getDirectoryContents "../sample"
  files <- filterM doesFileExist $ List.map (\f -> "../sample/"++f) dirContents
  indexes <- mapConcurrently loadFile files
  return $ Map.unionsWith mergeEntries indexes
