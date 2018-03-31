module Main where

import           Control.Monad                 (when)
import qualified Data.HashMap.Strict           as HashMap

import qualified Data.PerfectHash.Construction as Construction
import qualified Data.PerfectHash.Lookup       as Lookup
import qualified Exercise


enableDebug = False

dictionaryPath = "/usr/share/dict/words"


main = do

  word_index_tuples <- Exercise.wordsFromFile dictionaryPath

  putStrLn $ unwords ["Words size:", show $ length word_index_tuples]

  let lookup_table = Construction.createMinimalPerfectHash $ HashMap.fromList word_index_tuples

  putStrLn $ unwords [
      "Finished computing lookup table with"
    , show $ Lookup.size lookup_table
    , "entries."
    ]

  when enableDebug $ do
    putStrLn $ unwords ["Vector G:", show $ Lookup.nonces lookup_table]
    putStrLn $ unwords ["Vector V:", show $ Lookup.values lookup_table]

  Exercise.eitherExit $ Exercise.testPerfectLookups lookup_table $ HashMap.fromList word_index_tuples
