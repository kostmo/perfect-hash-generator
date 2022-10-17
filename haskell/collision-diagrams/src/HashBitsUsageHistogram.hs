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


data MatrixVal = MatrixVal Int BiasIndicator


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
  

doThing :: BitSizes -> Matrix MatrixVal
doThing bit_sizes =
  fmap f myMatrix
  where
    f val = MatrixVal val $ mkBiasIndicator max_equal_count val
    max_equal_count = distinctInputValueCount bit_sizes

    myMatrix = makeMatrix
      bit_sizes
      (Hashing.getHash . Hashing.modernHash Nothing)
      [0..maxInputValue bit_sizes]


data InputOutputPair = InputOutputPair { 
    inputVal :: Int
  , outputVal :: Word32
  }


-- | Matrix is in row-major order.
-- Each row constitutes the bits of the hash output.
-- Row index corresponds to bit positions of the
-- input value.
generateCorrelationMatrix :: BitSizes -> InputOutputPair -> Matrix Int
generateCorrelationMatrix (BitSizes row_count col_count) (InputOutputPair input_data_val hash_val) =
  M.matrix row_count col_count f
  where
    f (row_index, col_index) = fromEnum $
      testBit hash_val col_index == testBit input_data_val row_index


makeMatrix :: BitSizes -> HashFunction -> [Int] -> Matrix Int
makeMatrix b@(BitSizes row_count col_count) hashFunction =
  foldl' f $ M.zero row_count col_count
  where
    f x = M.elementwise (+) x . makeVal

    makeVal input = generateCorrelationMatrix b $ InputOutputPair input $ hashFunction input