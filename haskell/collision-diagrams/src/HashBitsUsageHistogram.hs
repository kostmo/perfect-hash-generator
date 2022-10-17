module HashBitsUsageHistogram where

import qualified Data.Matrix           as M
import  Data.Matrix           (Matrix)
import Data.Foldable           (foldl')
import Data.Word (Word32)
import Data.Bits (testBit)

import qualified Data.PerfectHash.Hashing as Hashing


data BitSizes = BitSizes {
    inputBitsCount :: Int -- ^ row count
  , hashBitsCount :: Int -- ^ col count
  }


data BiasIndicator =
    AlwaysSame
  | AlwaysDifferent
  | Correlation Double 


type HashFunction = Int -> Word32

type CellFunction = InputOutputPair -> (Int, Int) -> Int


data MatrixVal = MatrixVal Int BiasIndicator


data InputOutputPair = InputOutputPair { 
    inputVal :: Int
  , outputVal :: Word32
  }



maxInputValue :: BitSizes -> Int
maxInputValue bit_sizes = distinctInputValueCount bit_sizes - 1


distinctInputValueCount :: BitSizes -> Int
distinctInputValueCount bit_sizes = 2^data_bitwidth
  where
    data_bitwidth = inputBitsCount bit_sizes


mkBiasIndicator :: Int -> Int -> BiasIndicator
mkBiasIndicator max_equal_count actual_val
  | actual_val == max_equal_count = AlwaysSame
  | actual_val == 0 = AlwaysDifferent
  | otherwise = Correlation $
    (fromIntegral actual_val - midpoint) / midpoint
    where
      midpoint = fromIntegral max_equal_count / 2
  

generateMatrixForHash :: BitSizes -> Matrix MatrixVal
generateMatrixForHash bit_sizes =
  fmap f myMatrix
  where
    f val = MatrixVal val $ mkBiasIndicator max_equal_count val
    max_equal_count = length input_list

    myMatrix = makeMatrix
      bit_sizes
      hashCollisionsCellFunction
      h
      input_list
      
    input_list = [0..maxInputValue bit_sizes]

    -- main_hash = Hashing.modernHash
    main_hash = Hashing.hash32 Hashing.modernFNV1aParms {
        Hashing.innerParams = (Hashing.innerParams Hashing.modernFNV1aParms) {
          -- Hashing.magicPrime = Hashing.Hash 0x100011b
          -- Hashing.magicPrime = Hashing.Hash 0x1001010 -- This fixes the "stuck" LSB
          Hashing.magicPrime = Hashing.primeFNV1a32bit
        }
      }

    h = Hashing.getHash . main_hash Nothing


generateMatrixForModulus :: Int -> BitSizes -> Matrix MatrixVal
generateMatrixForModulus modulus_value bit_sizes =
  fmap f myMatrix
  where
    f val = MatrixVal val $ mkBiasIndicator max_equal_count val
    max_equal_count = length input_list

    myMatrix = makeMatrix
      bit_sizes
      hashCollisionsCellFunction
      h
      input_list

    h x = fromIntegral $ x `mod` modulus_value

    input_list = [0..4 * maxInputValue bit_sizes]


hashCollisionsCellFunction :: InputOutputPair -> (Int, Int) -> Int
hashCollisionsCellFunction
    (InputOutputPair input_data_val hash_val)
    (one_based_row_index, one_based_col_index) =
  
  fromEnum equality
  where
    input_bit_index = one_based_row_index - 1
    output_bit_index = one_based_col_index - 1

    hash_bit = testBit hash_val output_bit_index
    input_bit = testBit input_data_val input_bit_index

    equality = hash_bit == input_bit


makeMatrix
  :: BitSizes
  -> CellFunction
  -> HashFunction
  -> [Int]
  -> Matrix Int
makeMatrix (BitSizes row_count col_count) cellFunction hashFunction =
  foldl' f $ M.zero row_count col_count
  where
    f x = M.elementwise (+) x . makeVal

    makeVal input = M.matrix row_count col_count $
      cellFunction $ InputOutputPair input $ hashFunction input
