{-# OPTIONS_HADDOCK prune #-}

-- | Constructs a minimal perfect hash from a map of key-value pairs.
--
-- Implementation was adapted from
-- <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's Blog>.
-- A refactoring of that Python implementation may be found
-- <https://github.com/kostmo/perfect-hash-generator/blob/master/python/perfect-hash.py here>.
-- This Haskell implementation was transliterated and evolved from that refactoring.
--
-- = Overview of algorithm
-- A two-input hash function @F(nonce, key)@ is used.
--
-- 1. Keys are hashed into buckets for the first round with a nonce of @0@.
-- 1. Iterating over each bucket of size >= 2 in order of decreasing size, keep testing different nonce values such that all members
--    of the bucket fall into open slots in the final array.
--    When a successful nonce is found, write it to the \"intermediate\" array at the bucket's position.
-- 1. For each bucket of size 1, select an arbitrary open slot in the final array, and write the slot's
--    index (after negation and subtracting 1) to the intermediate array.
--
-- According to <http://cmph.sourceforge.net/papers/esa09.pdf this paper>,
-- the algorithm is assured to run in linear time.
module Data.PerfectHash.Construction (
    createMinimalPerfectHash
  ) where

import           Control.Monad            (join)
import Data.SortedList (SortedList, toSortedList, fromSortedList)
import           Data.Default             (Default, def)
import           Data.Foldable            (foldl')
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import Data.Function (on)
import           Data.Ord                 (Down (Down))
import qualified Data.Vector.Unboxed      as Vector
import qualified Data.Maybe               as Maybe

import qualified Data.PerfectHash.Hashing as Hashing
import Data.PerfectHash.Hashing (Hash, Nonce, Size)
import qualified Data.PerfectHash.Lookup as Lookup


-- | NOTE: Vector may peform better for these structures, but
-- the code may not be as clean.
data LookupTable a = NewLookupTable {
    redirs :: HashMap Int Int
  , vals   :: HashMap Int a
  }


data SingletonBucket a = SingletonBucket Hash a
  deriving Eq


data HashBucket a = HashBucket {
    _hashVal :: Hash
  , bucketMembers :: [a]
  } deriving Eq

instance (Eq a) => Ord (HashBucket a) where 
  compare = compare `on` (Down . length . bucketMembers)


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
-- yields a collision when using the new nonce.
attemptNonceRecursive
  :: Hashing.ToHashableChunks a
  => HashMapAndSize Int b
  -> Nonce
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


-- | slots for each bucket, with the current nonce attempt
data PlacementAttempt a = PlacementAttempt Nonce [SingletonBucket a]


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
  -> PlacementAttempt a
findNonceForBucket nonce_attempt values_and_size bucket =

  -- This is a "lazy" way to specify recursion:
  -- If the result ("result_for_this_iteration") at this iteration of the recursion
  -- is not "Nothing", then, wrap it in a "PlacementAttempt" record.
  -- Otherwise, descend one layer deeper by computing "recursive_result".
  maybe
    recursive_result
    f
    result_for_this_iteration

  where
    f = PlacementAttempt nonce_attempt . flip (zipWith SingletonBucket) bucket

    result_for_this_iteration = sequenceA $ attemptNonceRecursive
      values_and_size
      nonce_attempt
      mempty
      bucket

    recursive_result = findNonceForBucket
      (nonce_attempt + 1)
      values_and_size
      bucket


-- | Searches for a nonce for this bucket, starting with the value @1@,
-- until one is found that results in no collisions for both this bucket
-- and all previous buckets.
handleMultiBuckets
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMapAndSize a b
  -> LookupTable b
  -> HashBucket a
  -> LookupTable b
handleMultiBuckets
    sized_words_dict
    old_lookup_table
    (HashBucket computed_hash bucket) =

  NewLookupTable new_g new_values_dict
  where
    HashMapAndSize words_dict size = sized_words_dict

    sized_vals_dict = HashMapAndSize (vals old_lookup_table) size

    PlacementAttempt nonce slots_for_bucket =
      findNonceForBucket 1 sized_vals_dict bucket

    new_g = HashMap.insert computed_hash nonce $ redirs old_lookup_table
    new_values_dict = foldr f (vals old_lookup_table) slots_for_bucket

    f (SingletonBucket slot_val bucket_val) = HashMap.insert slot_val $
      HashMap.lookupDefault (error "not found") bucket_val words_dict


-- | This function exploits the sorted structure of the list
-- by skimming the multi-entry buckets from the front of the
-- list. Then we filter the single-entry buckets by dropping
-- the empty buckets.
findCollisionNonces
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMapAndSize a b
  -> SortedList (HashBucket a)
  -> (LookupTable b, [SingletonBucket a])
findCollisionNonces sized_words_dict sorted_bucket_hash_tuples =

  (lookup_table, remaining_words)
  where

    -- Since the buckets have been sorted by descending size,
    -- once we get to the bucket with 1 or fewer elements,
    -- we know there are no more collision buckets.
    (multi_entry_buckets, single_or_fewer_buckets) =
      span ((> 1) . length . bucketMembers) $
        fromSortedList sorted_bucket_hash_tuples

    -- XXX Using 'foldl' rather than 'foldr' is crucial here, given the order
    -- of the buckets. 'foldr' would actually try to place the smallest buckets
    -- first, making it improbable that the large buckets will be placeable,
    -- and potentially resulting in an infinite loop.
    lookup_table = foldl'
      (handleMultiBuckets sized_words_dict)
      emptyLookupTable
      multi_entry_buckets

    remaining_words = Maybe.mapMaybe
      convertToSingletonBucket
      single_or_fewer_buckets

    convertToSingletonBucket :: HashBucket a -> Maybe (SingletonBucket a)
    convertToSingletonBucket (HashBucket hashVal elements) = do
      first_elem <- Maybe.listToMaybe elements
      return $ SingletonBucket hashVal first_elem


-- | Sort buckets by descending size
preliminaryBucketPlacement
  :: (Hashing.ToHashableChunks a, Eq a, Hashable a)
  => HashMap a b
  -> SortedList (HashBucket a)
preliminaryBucketPlacement words_dict =
  toSortedList bucket_hash_tuples
  where
    size = HashMap.size words_dict
    slot_key_pairs = deriveTuples
      (Hashing.hashToSlot 0 size) $
        HashMap.keys words_dict

    bucket_hash_tuples = map (uncurry HashBucket) $
      HashMap.toList $ binTuplesBySecond slot_key_pairs


-- | Generates a minimal perfect hash for a set of key-value pairs.
--
-- The keys must be instances of 'Hashing.ToHashableChunks'.
-- The values may be of arbitrary type.
--
-- A 'HashMap' is required as input to guarantee that there are
-- no duplicate keys.
createMinimalPerfectHash
  :: (Hashing.ToHashableChunks k, Eq k, Hashable k, Vector.Unbox v, Default v)
  => HashMap k v -- ^ key-value pairs
  -> Lookup.LookupTable v
     -- ^ output for use by 'LookupTable.lookup' or a custom code generator
createMinimalPerfectHash words_dict =
  convertToVector $ NewLookupTable final_g final_values
  where
    size = HashMap.size words_dict

    sorted_bucket_hash_tuples = preliminaryBucketPlacement words_dict

    -- TODO: Rename this variable: "remaining_word_hash_tuples"
    (intermediate_lookup_table, remaining_word_hash_tuples) =
      findCollisionNonces
        (HashMapAndSize words_dict size)
        sorted_bucket_hash_tuples

    isUnusedSlot = not . (`HashMap.member` vals intermediate_lookup_table)

    unused_slots = filter
      isUnusedSlot
      [0..(size - 1)]

    zipped_remaining_with_unused_slots =
      zip remaining_word_hash_tuples unused_slots

    -- Note: We subtract one to ensure it's negative even if the
    -- zeroeth slot was used.
    f1 (SingletonBucket computed_hash _, free_slot_index) =
      HashMap.insert computed_hash $
        Lookup.encodeDirectEntry free_slot_index

    final_g = foldr
      f1
      (redirs intermediate_lookup_table)
      zipped_remaining_with_unused_slots

    f2 (SingletonBucket _ word, free_slot_index) =
      HashMap.insert free_slot_index $
        HashMap.lookupDefault (error "Impossible!") word words_dict

    final_values = foldr
      f2
      (vals intermediate_lookup_table)
      zipped_remaining_with_unused_slots


-- * Utility functions

-- | Place the first elements of the tuples into bins according to the second
-- element.
binTuplesBySecond
  :: (Foldable t, Eq b, Hashable b)
  => t (a, b)
  -> HashMap.HashMap b [a]
binTuplesBySecond = foldr f HashMap.empty
  where
    f tuple = HashMap.insertWith mappend (snd tuple) $ pure $ fst tuple


-- | duplicates the argument into both members of the tuple
duple :: a -> (a, a)
duple = join (,)


-- | Given a function and a value, create a pair
-- where the first element is the value, and the
-- second element is the function applied to the value
derivePair :: (a -> b) -> a -> (a, b)
derivePair g = fmap g . duple


deriveTuples :: Functor t => (a -> b) -> t a -> t (a, b)
deriveTuples = fmap . derivePair
