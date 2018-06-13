{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeSynonymInstances #-}


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


-- | This choice of prime number @0x01000193@ was taken from the Python implementation
-- on <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's page>.
primeFNV :: Int
primeFNV = 0x01000193


mask32bits :: Int
mask32bits = 0xffffffff


-- | Mechanism for a key to be decomposed into units processable by the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV-1a> hashing algorithm.
class ToHashableChunks a where
  toHashableChunks :: a -> [Int]

instance ToHashableChunks Int where
  toHashableChunks = map fromIntegral . B.unpack . encode

instance ToHashableChunks String where
  toHashableChunks = map ord

instance ToHashableChunks Text where
  toHashableChunks = toHashableChunks . T.unpack


hashToSlot :: ToHashableChunks a =>
     Int -- ^ nonce
  -> a -- ^ key
  -> Int -- ^ array size
  -> Int
hashToSlot nonce key size = hash nonce key `mod` size


-- | Uses the \"FNV-1a\" algorithm from the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV website>:
--
-- > hash = offset_basis
-- > for each octet_of_data to be hashed
-- >         hash = hash xor octet_of_data
-- >         hash = hash * FNV_prime
-- > return hash
--
-- The interface is comparable to the
-- <https://hackage.haskell.org/package/hashable-1.2.6.1/docs/Data-Hashable.html#v:hashWithSalt hashWithSalt>
-- function from the @hashable@ package.
hash :: ToHashableChunks a =>
     Int -- ^ nonce
  -> a -- ^ key
  -> Int
hash nonce =

  -- NOTE: This must be 'foldl', not 'foldr'
  foldl' combine d . toHashableChunks
  where
    d = if nonce == 0
      then primeFNV
      else nonce

    combine acc = (.&. mask32bits) . (* primeFNV) . xor acc
