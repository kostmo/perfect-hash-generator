{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default                   (Default)
import           Data.Either                    (isRight)
import           Data.Hashable                  (Hashable)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import           Data.Text                      (Text)
import qualified Data.Vector.Unboxed            as Vector
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)

import qualified Data.PerfectHash.Construction  as Construction
import qualified Data.PerfectHash.Hashing       as Hashing
import qualified Exercise


testHashComputation
  :: (Hashing.ToHashableChunks a, Show a)
  => a
  -> Int
  -> IO ()
testHashComputation key val =
  assertEqual error_message val computed_hash
  where
    error_message = unwords ["Incorrect hash computation of", show key]
    computed_hash = Hashing.hash 0 key


mkInputs
  :: (Eq a, Hashable a)
  => [a]
  -> HashMap a Int
mkInputs inputs = HashMap.fromList $ zip inputs [1..]


wordIndexTuplesString :: HashMap String Int
wordIndexTuplesString = mkInputs [
    "apple"
  , "banana"
  , "carrot"
  ]


wordIndexTuplesText :: HashMap Text Int
wordIndexTuplesText = mkInputs [
    "alpha"
  , "beta"
  , "gamma"
  ]


intMapTuples :: HashMap Int Int
intMapTuples = mkInputs [
    1000
  , 5555
  , 9876
  ]


testHashLookups
  :: (Show a, Show b, Eq b, Vector.Unbox b, Default b, Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMap a b
  -> IO ()
testHashLookups word_index_tuples =
  assertBool "Perfect hash lookups failed to match the input" $ isRight test_result_either
  where
    lookup_table = Construction.createMinimalPerfectHash word_index_tuples
    test_result_either = Exercise.testPerfectLookups lookup_table word_index_tuples


tests = [
    testGroup "Hash computation" [
      testCase "compute-string-hash" $ testHashComputation ("blarg" :: String) 3322346319
    , testCase "compute-int-hash" $ testHashComputation (70000 :: Int) 4169891409
    ]
  , testGroup "Hash lookups" [
      testCase "word-lookups-string" $ testHashLookups wordIndexTuplesString
    , testCase "word-lookups-text" $ testHashLookups wordIndexTuplesText
    , testCase "int-lookups" $ testHashLookups intMapTuples
    ]
  ]


main = defaultMain tests
