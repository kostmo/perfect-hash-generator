-- | This module is not used by the main library for constructing perfect hashes;
-- it just provides convenience functions for use by the tests and demonstration executables.
module Exercise where

import           Control.Monad            (unless)
import           Data.Foldable            (traverse_)
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Vector.Unboxed      as Vector

import qualified Data.PerfectHash.Hashing as Hashing
import qualified Data.PerfectHash.Lookup  as Lookup


-- | genericized to facilitate benchmarking
testLookupsHelper :: (Show b, Eq b, Show a, Hashing.ToHashableChunks a, Vector.Unbox b) =>
     (a -> b) -- ^ lookup function
  -> HashMap a b
  -> Either String ()
testLookupsHelper lookup_function =
  traverse_ check_entry . HashMap.toList
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
        lookup_result = lookup_function word


testHashMapLookups :: (Show b, Eq b, Show a, Eq a, Hashable a, Hashing.ToHashableChunks a, Vector.Unbox b) =>
     HashMap a b
  -> Either String ()
testHashMapLookups hash_map = testLookupsHelper (\x -> HashMap.lookupDefault (error "not found") x hash_map) hash_map


testPerfectLookups :: (Show b, Eq b, Show a, Hashing.ToHashableChunks a, Vector.Unbox b) =>
     Lookup.LookupTable b
  -> HashMap a b
  -> Either String ()
testPerfectLookups = testLookupsHelper . Lookup.lookup


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
