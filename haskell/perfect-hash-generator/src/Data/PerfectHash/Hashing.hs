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
import           Data.Bits            (shiftR, xor, (.&.))
import qualified Data.ByteString.Lazy as B (unpack)
import           Data.Char            (ord)
import           Data.Foldable        (foldl')
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.PerfectHash.Types.Nonces as Nonces
import Data.PerfectHash.Types.Nonces (Nonce)
import           Text.Printf
import Data.Word (Word32, Word16)


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
  deriving Eq


type Hash32 = Hash Word32

type Hash16 = Hash Word16

instance Show Hash32 where
  show x = "<" <> unwords [hex_rep, "(" <> binary_rep <> ")"] <> ">"
    where
      hex_rep = printf "0x%08x" $ getHash x
      binary_rep = printf "0b%032b" $ getHash x


newtype SlotIndex = SlotIndex {getIndex :: Int}
  deriving (Eq, Show)

newtype ArraySize = ArraySize Int
  deriving Show

-- | This type is strictly for debugging; it carries the SlotIndex
-- (the necessary datum) as well as the original hash
-- (the debugging datum).
data HashSlot = HashSlot {
    getSlot :: SlotIndex
  , getOriginalHash :: Hash32
  } deriving (Eq, Show)


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
  -> HashSlot
hashToSlot hash_function maybe_nonce (ArraySize size) key =
  HashSlot slot h
  where
    h = hash_function maybe_nonce key
    slot = SlotIndex $ fromIntegral (getHash h) `mod` size


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


mask16bits :: Word32
mask16bits = 0xffff


-- | Note that the FNV algorithm is subject to the following shortcoming
-- regarding "diffusion", described
-- <https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-17.html#section-7.1 here>:
--
-- > Diffusion - Every output bit of a cryptographic hash should be an
-- > equally complex function of every input bit. But it is easy to see
-- > that the least significant bit of a direct FNV hash is the XOR of
-- > the least significant bits of every input byte and does not depend
-- > on any other input bit. While more complex, the second through
-- > seventh least significant bits of an FNV hash have a similar
-- > weakness; only the top bit of the bottom byte of output, and
-- > higher order bits, depend on all input bits. If these properties
-- > are considered a problem, they can be easily fixed by XOR folding
-- > (see <https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-17.html#section-7.1 Section 3>).
--
-- Also see "xor-folding" technique described
-- <http://isthe.com/chongo/tech/comp/fnv/#xor-fold here>.
foldHash16 :: Hash32 -> Hash16
foldHash16 (Hash old_hash) = Hash $ fromIntegral $
  (old_hash `shiftR` 16) `xor` (old_hash .&. mask16bits)

