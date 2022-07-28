{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}


-- | Implements the specialized hash function for
-- this perfect hashing algorithm.
--
-- C code that makes use of the perfect hash table output must exactly
-- re-implement this 'hash' function.
module Data.PerfectHash.Hashing where

import           Data.Binary          (encode)
import           Data.Bits            (xor, (.&.))
import qualified Data.ByteString.Lazy as B (unpack)
import           Data.Char            (ord)
import           Data.Foldable        (foldl')
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.PerfectHash.Types.Nonces as Nonces
import Data.PerfectHash.Types.Nonces (Nonce)
import Data.Word (Word32)


-- Types

-- | The nonce (i.e. the "secondary input") to the hash function
-- is of 'Maybe' type, so that the /Initial Basis/ value is selected
-- when no nonce ('Nothing') is provided.
--
-- We would like to keep knowledge of the actual initial basis
-- value local to the hashing function itself, instead of
-- requiring callsites to know which initial basis value to
-- use when the nonce would otherwise be zero.
type HashFunction a b = Maybe Nonce -> a -> b

newtype Hash a = Hash {getHash :: a}
  deriving (Eq, Show)

type Hash32 = Hash Word32

newtype SlotIndex = SlotIndex {getIndex :: Int}

newtype ArraySize = ArraySize Int
  deriving Show


-- | Parameters for FNV hashing algorithm
-- See http://isthe.com/chongo/tech/comp/fnv/
--
-- Parameterized to bit width
data FNVParams a = FNVParams {
    initialBasis :: Hash a
  , innerParams :: FNVInnerParams a
  }

-- | Inner FNV parameters
data FNVInnerParams a = FNVInnerParams {
    magicPrime :: Hash a
  , bitmask :: a
  }


-- * Constants

-- | This choice of prime number @0x01000193@ was taken from the Python implementation
-- on <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's page>.
primeFNV1a32bit :: Hash32
primeFNV1a32bit = Hash 0x01000193


-- | See the <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV website> for details
initialBasisFNV1a32bit :: Hash32
initialBasisFNV1a32bit = Hash 0x811c9dc5


mask32bits :: Word32
mask32bits = 0xffffffff


modernFNV1aParms :: FNVParams Word32
modernFNV1aParms = FNVParams {
    initialBasis = initialBasisFNV1a32bit
  , innerParams = FNVInnerParams {
      magicPrime = primeFNV1a32bit
    , bitmask = mask32bits
    }
  }


modernHash :: ToOctets a => HashFunction a Hash32
modernHash = hash32 modernFNV1aParms


-- * Class instances

-- | Mechanism for a key to be decomposed into units processable by the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV-1a> hashing algorithm.
class ToOctets a where
  toOctets :: a -> [Hash32]

-- | Drops all leading zero-bytes
instance ToOctets Int where
  toOctets = map Hash . dropWhile (== 0) . map fromIntegral . B.unpack . encode

instance ToOctets String where
  toOctets = map $ Hash . fromIntegral . ord

instance ToOctets Text where
  toOctets = toOctets . T.unpack


-- Utilities

generateArrayIndices :: ArraySize -> [SlotIndex]
generateArrayIndices (ArraySize size) = map SlotIndex [0..(size - 1)]


-- * Main functions

hashToSlot
  :: ToOctets a
  => HashFunction a Hash32
  -> Maybe Nonce
  -> ArraySize
  -> a -- ^ key
  -> SlotIndex
hashToSlot hash_function maybe_nonce (ArraySize size) key =
  SlotIndex $ fromIntegral (getHash (hash_function maybe_nonce key)) `mod` size


-- | The interface is comparable to the
-- <https://hackage.haskell.org/package/hashable-1.2.6.1/docs/Data-Hashable.html#v:hashWithSalt hashWithSalt>
-- function from the @hashable@ package.
--
-- Uses the \"FNV-1a\" algorithm from the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV website>:
--
-- > hash = initial_basis
-- > for each octet_of_data to be hashed
-- >         hash = hash xor octet_of_data
-- >         hash = hash * FNV_prime
-- > return hash
hash32 :: ToOctets a =>
     FNVParams Word32
  -> HashFunction a Hash32
hash32 (FNVParams (Hash initial_basis) innerParams) maybe_nonce =
  hash32inner innerParams basis
  where
    basis = case maybe_nonce of
      Just (Nonces.Nonce nonce) -> fromIntegral nonce
      Nothing -> initial_basis


hash32inner
  :: ToOctets a
  => FNVInnerParams Word32
  -> Word32 -- ^ initial basis
  -> a
  -> Hash32
hash32inner (FNVInnerParams (Hash magic_prime) bitmask) basis =

  Hash . foldl' combine basis . toOctets
  where
    combine acc = (.&. bitmask) . (* magic_prime) . xor acc . getHash
