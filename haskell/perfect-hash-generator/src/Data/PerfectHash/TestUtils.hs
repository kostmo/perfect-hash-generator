-- | This module is not used by the main library for constructing perfect hashes;
-- it just provides convenience functions for use by the tests and demonstration executables.
module Data.PerfectHash.TestUtils where

import           Control.Monad            (unless)
import           Data.Foldable            (traverse_)
import qualified Data.Vector.Unboxed      as Vector

import qualified Data.PerfectHash.Hashing as Hashing
import qualified Data.PerfectHash.Lookup  as Lookup


testLookups :: (Show b, Eq b, Show a, Hashing.ToHashableChunks a, Vector.Unbox b) =>
     Lookup.LookupTable b
  -> [(a, b)]
  -> Either String ()
testLookups lookup_table =
  traverse_ check_entry
  where
    check_entry (word, source_index) = unless (lookup_result == source_index) $
      Left $ unwords [
          "Result for key"
        , show word
        , "had incorrect index"
        , show lookup_result
        , "; should have been"
        , show source_index
        ]
      where
        lookup_result = Lookup.lookupPerfect lookup_table word


-- | Generate a map of words from a file to their line numbers.
--
-- Intended for use with @\"/usr/share/dict/words\"@.
wordsFromFile :: FilePath -> IO [(String, Int)]
wordsFromFile path = do
  file_lines <- readFile path
  let word_index_tuples = zip (lines file_lines) [1..]
  return word_index_tuples


eitherExit :: Either String b -> IO ()
eitherExit x = case x of
  Left err -> error err
  Right _ -> return ()
