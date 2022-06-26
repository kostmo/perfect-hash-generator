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


newtype SlotIndex = SlotIndex {getIndex :: Int}

type Hash = Int

newtype ArraySize = ArraySize Int
  deriving Show


generateArrayIndices :: ArraySize -> [SlotIndex]
generateArrayIndices (ArraySize size) = map SlotIndex [0..(size - 1)]


mask32bits :: Int
mask32bits = 0xffffffff


-- | Mechanism for a key to be decomposed into units processable by the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV-1a> hashing algorithm.
class ToHashableChunks a where
  toHashableChunks :: a -> [Hash]

instance ToHashableChunks Int where
  toHashableChunks = map fromIntegral . B.unpack . encode

instance ToHashableChunks String where
  toHashableChunks = map ord

instance ToHashableChunks Text where
  toHashableChunks = toHashableChunks . T.unpack


hashToSlot :: ToHashableChunks a =>
     Nonce
  -> ArraySize
  -> a -- ^ key
  -> SlotIndex
hashToSlot nonce (ArraySize size) key =
  SlotIndex $ hash nonce key `mod` size


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
hash :: ToHashableChunks a =>
     Nonce -- ^ nonce
  -> a -- ^ key
  -> Hash
hash nonce =

  -- NOTE: This must be 'foldl', not 'foldr'
  foldl' combine d . toHashableChunks
  where
    d = Nonces.getNonzeroNonceVal nonce

    combine acc = (.&. mask32bits) . (* Nonces.primeFNV) . xor acc
