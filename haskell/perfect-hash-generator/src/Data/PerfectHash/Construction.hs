{-# OPTIONS_HADDOCK prune #-}

-- | Constructs a minimal perfect hash from a map of key-value pairs.
--
-- Implementation was adapted from
-- <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's Blog>.
--
-- A refactoring of that Python implementation may be found
-- <https://github.com/kostmo/perfect-hash-generator/blob/master/python/perfect-hash.py here>.
-- This Haskell implementation is transliterated from that refactoring.
module Data.PerfectHash.Construction (
    createMinimalPerfectHash
  ) where

import           Control.Monad            (join)
import           Data.Default             (Default, def)
import           Data.Foldable            (foldl')
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.List                (sortOn)
import           Data.Ord                 (Down (Down))
import qualified Data.Vector.Unboxed      as Vector
import qualified Data.Maybe               as Maybe

import qualified Data.PerfectHash.Hashing as Hashing
import Data.PerfectHash.Hashing (Hash, Nonce)
import qualified Data.PerfectHash.Lookup as Lookup
import Data.PerfectHash.Lookup (Size)


-- | NOTE: Vector may peform better for these structures, but
-- the code may not be as clean.
data LookupTable a = NewLookupTable {
    redirs :: HashMap Int Int
  , vals   :: HashMap Int a
  }


emptyLookupTable :: LookupTable a
emptyLookupTable = NewLookupTable mempty mempty


data HashMapAndSize a b = HashMapAndSize (HashMap a b) Size


convertToVector
  :: (Vector.Unbox a, Default a)
  => LookupTable a
  -> Lookup.LookupTable a
convertToVector x = Lookup.LookupTable a1 a2
  where
    size = length $ vals x
    vectorize defaultVal =
      Vector.generate size . flip (HashMap.lookupDefault defaultVal)
    a1 = vectorize 0 $ redirs x
    a2 = vectorize def $ vals x


-- | Computes a slot in the destination array (Data.PerfectHash.Lookup.values)
-- for every element in this multi-entry bucket, for the given nonce.
--
-- Return a Nothing for a slot if it collides.
--
-- This function is able to fail fast if one of the elements of the bucket
-- yields a collision with the new nonce.
attemptNonceRecursive
  :: Hashing.ToHashableChunks a
  => HashMapAndSize Int b
  -> Nonce -- ^ nonce
  -> IntSet -- ^ occupied slots
  -> [a] -- ^ keys
  -> [Maybe Int]
attemptNonceRecursive _ _ _ [] = []
attemptNonceRecursive
    values_and_size
    nonce
    occupied_slots
    (current_key:remaining_bucket_keys) =

  if cannot_use_slot
    then pure Nothing
    else Just slot : recursive_result

  where
    HashMapAndSize values size = values_and_size
    slot = Hashing.hashToSlot nonce size current_key

    cannot_use_slot = IntSet.member slot occupied_slots || HashMap.member slot values

    recursive_result = attemptNonceRecursive
      values_and_size
      nonce
      (IntSet.insert slot occupied_slots)
      remaining_bucket_keys


-- | Repeatedly try different values of the nonce until we find a hash function
-- that places all items in the bucket into free slots.
--
-- Keeps trying forever, incrementing the candidate nonce by @1@ each time.
-- Theoretically we're guaranteed to eventually find a solution.
findNonceForBucket
  :: Hashing.ToHashableChunks a
  => Nonce -- ^ nonce to attempt
  -> HashMapAndSize Int b
  -> [a] -- ^ colliding keys for this bucket
  -> ([(Hash, a)], Nonce) -- ^ slots for each bucket, with the current nonce attempt
findNonceForBucket nonce_attempt values_and_size bucket =

  maybe recursive_result (\x -> (zip x bucket, nonce_attempt)) maybe_attempt_result
  where
    recursive_result = findNonceForBucket
      (nonce_attempt + 1)
      values_and_size bucket

    maybe_attempt_result = sequenceA $ attemptNonceRecursive
      values_and_size
      nonce_attempt
      mempty
      bucket


-- | Searches for a nonce for this bucket, starting with the value @1@,
-- until one is found that results in no collisions for both this bucket
-- and all previous buckets.
handleMultiBuckets
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMapAndSize a b
  -> LookupTable b
  -> (Hash, [a])
  -> LookupTable b
handleMultiBuckets sized_words_dict old_lookup_table (computed_hash, bucket) =
  NewLookupTable new_g new_values_dict
  where
    HashMapAndSize words_dict size = sized_words_dict

    sized_vals_dict = HashMapAndSize (vals old_lookup_table) size
    (slots_for_bucket, nonce) = findNonceForBucket 1 sized_vals_dict bucket

    new_g = HashMap.insert computed_hash nonce $ redirs old_lookup_table
    new_values_dict = foldr f (vals old_lookup_table) slots_for_bucket

    f (slot_val, bucket_val) = HashMap.insert slot_val $
      HashMap.lookupDefault (error "not found") bucket_val words_dict


-- | This function exploits the sorted structure of the list twice,
-- first by skimming the multi-entry buckets, then by skimming
-- the single-entry buckets and dropping the empty buckets.
findCollisionNonces
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMapAndSize a b
  -> [(Hash, [a])]
  -> (LookupTable b, [(Hash, a)])
findCollisionNonces sized_words_dict sorted_bucket_hash_tuples =

  (lookup_table, remaining_words)
  where

    -- Since the buckets have been sorted by descending size,
    -- once we get to the bucket with 1 or fewer elements,
    -- we know there are no more collision buckets.
    (multi_entry_buckets, single_or_fewer_buckets) =
      span ((> 1) . length . snd) sorted_bucket_hash_tuples

    -- XXX Using 'foldl' rather than 'foldr' is crucial here, given the order
    -- of the buckets. 'foldr' would actually try to place the smallest buckets
    -- first, making it improbable that the large buckets will be placeable,
    -- and potentially resulting in an infinite loop.
    lookup_table = foldl'
      (handleMultiBuckets sized_words_dict)
      emptyLookupTable
      multi_entry_buckets

    remaining_words = Maybe.mapMaybe
      (traverse Maybe.listToMaybe)
      single_or_fewer_buckets


-- | Sort buckets by descending size
preliminaryBucketPlacement
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMap a b
  -> [(Hash, [a])]
preliminaryBucketPlacement words_dict =
  sortOn (Down . length . snd) bucket_hash_tuples
  where
    size = HashMap.size words_dict
    slot_key_pairs = deriveTuples
      (Hashing.hashToSlot 0 size) $
        HashMap.keys words_dict

    bucket_hash_tuples = HashMap.toList $ binTuplesBySecond slot_key_pairs


-- | Generates a minimal perfect hash for a set of key-value pairs.
--
-- The keys must be instances of 'Hashing.ToHashableChunks'.
-- The values may be of arbitrary type.
--
-- A 'HashMap' is required as input to guarantee that there are
-- no duplicate keys.
createMinimalPerfectHash
  :: (Vector.Unbox b, Default b, Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMap a b -- ^ key-value pairs
  -> Lookup.LookupTable b
     -- ^ output for use by 'LookupTable.lookup' or a custom code generator
createMinimalPerfectHash words_dict =
  convertToVector $ NewLookupTable final_g final_values
  where
    size = HashMap.size words_dict

    sorted_bucket_hash_tuples = preliminaryBucketPlacement words_dict

    (intermediate_lookup_table, remaining_word_hash_tuples) = findCollisionNonces
      (HashMapAndSize words_dict size)
      sorted_bucket_hash_tuples

    unused_slots = filter
      (not . (`HashMap.member` vals intermediate_lookup_table))
      [0..(size - 1)]

    zipped_remaining_with_unused_slots =
      zip remaining_word_hash_tuples unused_slots

    -- We subtract one to ensure it's negative even if the zeroeth slot was used.
    f1 ((computed_hash, _), free_slot_index) =
      HashMap.insert computed_hash $ Lookup.encodeDirectEntry free_slot_index

    final_g = foldr
      f1
      (redirs intermediate_lookup_table)
      zipped_remaining_with_unused_slots

    f2 ((_, word), free_slot_index) = HashMap.insert free_slot_index $
      HashMap.lookupDefault (error "Impossible!") word words_dict

    final_values = foldr
      f2
      (vals intermediate_lookup_table)
      zipped_remaining_with_unused_slots


-- * Utilities

-- | Place the first elements of the tuples into bins according to the second
-- element.
binTuplesBySecond
  :: (Eq b, Hashable b)
  => [(a, b)]
  -> HashMap.HashMap b [a]
binTuplesBySecond = foldr f HashMap.empty
  where
    f tuple = HashMap.insertWith (++) (snd tuple) [fst tuple]


-- * Utility functions

-- | duplicates the argument into both members of the tuple
duple :: a -> (a, a)
duple = join (,)


-- | Given a function and a value, create a pair
-- where the first element is the value, and the
-- second element is the function applied to the value
derivePair :: (a -> b) -> a -> (a, b)
derivePair g = fmap g . duple


deriveTuples :: (a -> b) -> [a] -> [(a, b)]
deriveTuples = map . derivePair
