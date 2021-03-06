module Main where

import           System.Random                 (RandomGen, mkStdGen, random)

import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IntSet
import qualified Data.PerfectHash.Construction as Construction
import qualified Data.PerfectHash.Lookup       as Lookup
import qualified Data.Vector.Unboxed           as Vector
import qualified Exercise
import           System.CPUTime
import           Text.Printf


valueCount = 250000


data RandIntAccum t = RandIntAccum
  t -- ^ random number generator
  Int -- ^ max count
  IntSet -- ^ accumulated unique random numbers


-- | Since computing the size of the set is O(N), we
-- maintain the count separately.
getUniqueRandomIntegers :: RandomGen t => RandIntAccum t -> IntSet
getUniqueRandomIntegers (RandIntAccum std_gen count current_set) =

  if count == 0
    then current_set
    else getUniqueRandomIntegers newstate

  where
    (next_int, next_std_gen) = random std_gen

    a = RandIntAccum next_std_gen
    newstate = if IntSet.member next_int current_set
      then a count current_set
      else a (count - 1) (IntSet.insert next_int current_set)


intMapTuples :: HashMap Int Int
intMapTuples = HashMap.fromList $ zip random_ints [1..]
  where
    seed_value = RandIntAccum (mkStdGen 0) valueCount IntSet.empty
    random_ints = IntSet.toList $ getUniqueRandomIntegers seed_value


main = do
  putStrLn $ unwords ["Keys size:", show $ length intMapTuples]

  let lookup_table = Construction.createMinimalPerfectHash intMapTuples

  putStrLn $ unwords [
      "Finished computing lookup table with"
    , show $ Lookup.size lookup_table
    , "entries."
    ]

  let direct_mapping_nonces = Vector.filter (< 0) $ Lookup.nonces lookup_table
      direct_mapping_count = Vector.length direct_mapping_nonces
      total_count = length intMapTuples
      direct_mapping_percentage = 100 * direct_mapping_count `div` total_count

  putStrLn $ unwords [
      "There were"
    , show $ Vector.length direct_mapping_nonces
    , "(" ++ show direct_mapping_percentage ++ "%)"
    , "lookup entries with direct mappings."
    ]

  putStrLn "Testing perfect hash lookups..."
  start1 <- getCPUTime
  Exercise.eitherExit $ Exercise.testPerfectLookups lookup_table intMapTuples
  end1   <- getCPUTime

  let diff1 = fromIntegral (end1 - start1) / (10^12)
  putStrLn $ printf "Computation time: %0.3f sec\n" (diff1 :: Double)

  putStrLn "Testing HashMap lookups..."
  start2 <- getCPUTime
  Exercise.eitherExit $ Exercise.testHashMapLookups intMapTuples
  end2   <- getCPUTime

  let diff2 = fromIntegral (end2 - start2) / (10^12)
  putStrLn $ printf "Computation time: %0.3f sec\n" (diff2 :: Double)

  putStrLn "Done."
