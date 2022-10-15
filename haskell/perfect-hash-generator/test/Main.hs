{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either                    (isRight)
import qualified Data.Map as Map
import           Data.Map                 (Map)
import           Data.Text                      (Text)
import           Test.Framework                 (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertBool, assertEqual)

import qualified Data.PerfectHash.Construction  as Construction
import  Data.PerfectHash.Hashing       (Hash32)
import qualified Data.PerfectHash.Hashing       as Hashing
import qualified Data.PerfectHash.Lookup  as Lookup

import qualified Exercise


testHashComputation
  :: (Hashing.ToOctets a, Show a)
  => a
  -> Hash32
  -> IO ()
testHashComputation key val =
  assertEqual error_message val computed_hash
  where
    error_message = unwords ["Incorrect hash computation of", show key]
    computed_hash = Hashing.modernHash Nothing key


mkInputs
  :: Ord a
  => [a]
  -> Map a Int
mkInputs = Map.fromList . flip zip [1..]


wordIndexTuplesString :: Map String Int
wordIndexTuplesString = mkInputs [
    "apple"
  , "banana"
  , "carrot"
  ]


wordIndexTuplesText :: Map Text Int
wordIndexTuplesText = mkInputs [
    "alpha"
  , "beta"
  , "gamma"
  ]


testHashLookups
  :: (Show a, Show b, Eq b, Hashing.ToOctets a)
  => Map a b
  -> IO ()
testHashLookups word_index_tuples =
  assertBool "Perfect hash lookups failed to match the input" $ isRight test_result_either
  where
    lookup_table = Exercise.eitherError $ Construction.createMinimalPerfectHash word_index_tuples
    test_result_either = Exercise.testPerfectLookups lookup_table word_index_tuples


tests = [
    testGroup "Octets conversion" [  
      testCase "small int" $
        assertEqual
          "Incorrect Octets conversion"
          [Hashing.Hash 0x1b, Hashing.Hash 0x58]
          (Hashing.toOctets (7000 :: Int))
    ]      
  , testGroup "Hash computation" [
      testCase "compute-string-hash" $
        testHashComputation ("blarg" :: String) $ Hashing.Hash 0x4a6457b9
    , testCase "compute-int-hash" $
        testHashComputation (7000 :: Int) $ Hashing.Hash 0x6fb40186
    ]
  , testGroup "Hash lookups" [
      testCase "word-lookups-string" $ testHashLookups wordIndexTuplesString
    , testCase "word-lookups-text" $ testHashLookups wordIndexTuplesText
    , testCase "int-lookups" $ testHashLookups largeIntMapTuples
    ]
  , testGroup "Large scale round-tripping with random inputs" [
      testCase "integers" $ assertBool "Lookups failed to match input" $
        isRight $ Exercise.testPerfectLookups large_lookup_table largeIntMapTuples
    ]
  , testGroup "Missing key detection" keyVerificationTestCases
  ]
  where
    largeIntMapTuples = Exercise.mkIntMapTuples 100000
    large_lookup_table = Exercise.eitherError $
      Construction.createMinimalPerfectHash largeIntMapTuples


keyVerificationTestCases :: [Test]
keyVerificationTestCases = [
    testCase "Lookup attempt with invalid key (unchecked)" $
      assertEqual
        "Maps to an arbitrary, incorrect value"
        (lookupPlain ("d" :: String))
        2
  , testCase "Lookup attempt with a valid key" $
      assertEqual
        "Should report key found"
        (lookupVerified ("c" :: String))
        (Just 9)
  , testCase "Lookup attempt with an invalid key" $
      assertEqual
        "Should report key not found"
        (lookupVerified ("d" :: String))
        Nothing
  ]
  where
    smallMap = Map.fromList [
        ("a" :: String, 4 :: Int)
      , ("b", 2)
      , ("c", 9)
      ]
    smallLookupTable = Exercise.eitherError $ Construction.createMinimalPerfectHash smallMap
    lookupPlain = Lookup.lookup Hashing.modernHash smallLookupTable

    smallLookupTableStoredKeys = Exercise.eitherError $  Construction.createMinimalPerfectHashWithKeys smallMap
    lookupVerified = Lookup.lookupVerifyKey Hashing.modernHash smallLookupTableStoredKeys
    

main = defaultMain tests