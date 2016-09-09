module Lib where

import Data.Char
import Data.ByteString.Lazy as LBS
import Data.ByteString as BS
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Tar as Tar
import qualified Data.List as List
import qualified System.IO as SysIO
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text as Text

type WordMap = Map.Map Text.Text [(Text.Text,Int)]

-- Tokenizes a list by splitting on any non-letter character and then
-- deleting any empty values
-- the $ helps avoid parens, so x y $ a b means x y (a b)
tokenize :: Text.Text -> [Text.Text]
tokenize str =
  List.filter (\bs -> bs /= Text.empty) $ Text.split (not . isLetter) str

addWiki' :: WordMap -> Tar.EntryContent -> WordMap
addWiki' currMap (Tar.NormalFile textW8 _) =
  -- Make sure title has been evaluated (seq), then for each word in
  -- the temp map, add it to the main wordMap using addWord
  title `seq` List.foldl' (addWord title) currMap $ Map.toList tempWordMap
  where
    -- Create a temporary map of word -> count
    tempWordMap = words `seq` List.foldl' addTempWord Map.empty words
    -- Convert the lazy bytestring returned by tar into a regular
    -- bytestring, and then decode it into a Text (the concat & toChunks
    -- converts from the kind of ByteString that Tar uses to the kind
    -- that Text needs, kind of a hack)
    text = TextEnc.decodeUtf8 (BS.concat $ LBS.toChunks textW8)
    title = Text.takeWhile (\c -> (c /= '\n') && (c /= '\r')) text
    words = tokenize $ Text.toLower text
    -- Adds a word to the temporary map, if the word is already there,
    -- it adds the counts together, using `seq` to force evaluation of
    -- the counts
    addTempWord wordMap word = Map.insertWith (\n o -> n `seq` o `seq` (n+o)) word 1 wordMap
    -- Adds a (title,count) pair to the list of items associated with a
    -- word in the map
    addWord title wordMap (word,c) = Map.insertWith mergeLists word [(title,c)] wordMap
    mergeLists n o = n `seq` o `seq` (n++o)

-- If the tar file isn't Tar.NormalFile, just skip the entry and return
-- the current map
addWiki' currMap _ = currMap

addWiki :: WordMap -> Tar.Entry -> WordMap
addWiki currMap entry =
  -- Add the entry to the word map
  addWiki' currMap (Tar.entryContent entry)

load_wikipedia :: IO WordMap
load_wikipedia = do
  -- Lazily decompress the tar file as it is read
  uncompressed <- fmap BZip.decompress (LBS.readFile "../sample.tar.bz2")
  -- Starting with an empty map, call addWiki for each entry in the tar file
  let (Right d) = Tar.foldlEntries addWiki Map.empty (Tar.read uncompressed)
  return d

search :: Text.Text -> WordMap -> [(Text.Text,Int)]
search term index =
  -- Find the entry in the list, defaulting to the empty list, then
  -- sort the results
  List.sortOn sortFunc (Map.findWithDefault [] term index)
  where
    sortFunc (f,i) = -i
