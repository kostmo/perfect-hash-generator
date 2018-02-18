{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either                    (isRight)
import           Data.Hashable                  (Hashable)
import           Data.Text                      (Text)
import qualified Data.Vector.Unboxed            as Vector
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)

import qualified Data.PerfectHash.Construction  as Construction
import qualified Data.PerfectHash.Hashing       as Hashing
import qualified Exercise


testHashComputation :: (Hashing.ToHashableChunks a, Show a) =>
     a
  -> Int
  -> IO ()
testHashComputation key val =
  assertEqual error_message val computed_hash
  where
    error_message = unwords ["Incorrect hash computation of", show key]
    computed_hash = Hashing.hash 0 key


wordIndexTuplesString :: [(String, Int)]
wordIndexTuplesString = zip [
    "apple"
  , "banana"
  , "carrot"
  ] [1..]


wordIndexTuplesText :: [(Text, Int)]
wordIndexTuplesText = zip [
    "alpha"
  , "beta"
  , "gamma"
  ] [1..]


intMapTuples :: [(Int, Int)]
intMapTuples = [
    (1000, 1)
  , (5555, 2)
  , (9876, 3)
  ]


testHashLookups :: (Show a, Show b, Eq b, Vector.Unbox b, Construction.Defaultable b, Hashing.ToHashableChunks a, Eq a, Hashable a) =>
  [(a, b)] -> IO ()
testHashLookups word_index_tuples =
  assertBool "Perfect hash lookups failed to match the input" $ isRight test_result_either
  where
    lookup_table = Construction.createMinimalPerfectHash word_index_tuples
    test_result_either = Exercise.testLookups lookup_table word_index_tuples


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
