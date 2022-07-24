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


-- Types

type HashFunction a = Nonce -> a -> Hash


newtype SlotIndex = SlotIndex {getIndex :: Int}

newtype Hash = Hash {getHash :: Int}
  deriving (Eq, Show)

newtype ArraySize = ArraySize Int
  deriving Show

-- | Parameters for FVN hashing algorithm
-- See http://isthe.com/chongo/tech/comp/fnv/
data FNVParams = FNVParams {
    initialBasis :: Hash
  , magicPrime :: Hash
  }


-- * Constants

-- | This choice of prime number @0x01000193@ was taken from the Python implementation
-- on <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's page>.
primeFNV1a32bit :: Hash
primeFNV1a32bit = Hash 0x01000193


initialBasisFNV1a32bit :: Hash
initialBasisFNV1a32bit = Hash 0x811c9dc5


mask32bits :: Int
mask32bits = 0xffffffff


modernFNV1aParms :: FNVParams
modernFNV1aParms = FNVParams {
    initialBasis = initialBasisFNV1a32bit
  , magicPrime = primeFNV1a32bit
  }


modernHash :: ToOctets a => HashFunction a
modernHash = hash32 modernFNV1aParms


-- * Class instances

-- | Mechanism for a key to be decomposed into units processable by the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV-1a> hashing algorithm.
class ToOctets a where
  toOctets :: a -> [Hash]

instance ToOctets Int where
  toOctets = map Hash . dropWhile (== 0) . map fromIntegral . B.unpack . encode

instance ToOctets String where
  toOctets = map $ Hash . ord

instance ToOctets Text where
  toOctets = toOctets . T.unpack


-- Utilities

generateArrayIndices :: ArraySize -> [SlotIndex]
generateArrayIndices (ArraySize size) = map SlotIndex [0..(size - 1)]


-- * Main functions

hashToSlot
  :: ToOctets a
  => HashFunction a
  -> Nonce
  -> ArraySize
  -> a -- ^ key
  -> SlotIndex
hashToSlot hash_function nonce (ArraySize size) key =
  SlotIndex $ getHash (hash_function nonce key) `mod` size


-- Used in the 'hash32' function
getNonzeroNonceVal :: FNVParams -> Nonce -> Int
getNonzeroNonceVal (FNVParams (Hash initial_basis) _) (Nonces.Nonce nonce) =
  if nonce == 0
    then initial_basis
    else nonce


-- | The interface is comparable to the
-- <https://hackage.haskell.org/package/hashable-1.2.6.1/docs/Data-Hashable.html#v:hashWithSalt hashWithSalt>
-- function from the @hashable@ package.
--
-- Uses the \"FNV-1a\" algorithm from the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV website>:
--
-- > hash = offset_basis
-- > for each octet_of_data to be hashed
-- >         hash = hash xor octet_of_data
-- >         hash = hash * FNV_prime
-- > return hash
hash32 :: ToOctets a =>
     FNVParams
  -> HashFunction a
hash32 parms@(FNVParams _ (Hash magic_prime)) nonce =

  -- NOTE: This must be 'foldl', not 'foldr'
  Hash . foldl' combine d . toOctets
  where
    d = getNonzeroNonceVal parms nonce

    combine acc = (.&. mask32bits) . (* magic_prime) . xor acc . getHash
