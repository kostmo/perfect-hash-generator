{-# OPTIONS_HADDOCK prune #-}

-- | Constructs a minimal perfect hash from a map of key-value pairs.
--
-- = Overview of algorithm
-- A two-input hash function @F(nonce, key)@ is used.
--
-- <<docs/images/algorithm-diagram.svg>>
--
-- 1. Keys are hashed into buckets for the first round with a nonce of @0@.
-- 1. Iterating over each bucket of size >= 2 in order of decreasing size, keep
--    testing different nonce values until all members
--    of the bucket fall into open slots in the final array.
--    When a successful nonce is found, write it to the \"intermediate\" array
--    at the bucket's position.
-- 1. For each bucket of size 1, select an arbitrary open slot in the final
--    array, and write the slot's
--    index (after negation and subtracting 1) to the intermediate array.
--
-- According to <http://cmph.sourceforge.net/papers/esa09.pdf this paper>,
-- the algorithm is assured to run in linear time.
--
-- = Provenance
-- This implementation was adapted from
-- <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's Blog>.
-- A refactoring of that Python implementation may be found
-- <https://github.com/kostmo/perfect-hash-generator/blob/master/python/perfect-hash.py here>.
-- This Haskell implementation was transliterated and evolved from that refactoring.
--
module Data.PerfectHash.Construction (
    createMinimalPerfectHash
  ) where

import Data.Tuple (swap)
import           Data.Default             (Default, def)
import           Control.Monad            (join)
import Data.SortedList (SortedList, toSortedList, fromSortedList)
import           Data.Foldable            (foldl')
import qualified Data.IntSet              as IntSet
import           Data.IntSet              (IntSet)
import qualified Data.IntMap              as IntMap
import           Data.IntMap              (IntMap)
import qualified Data.Map as Map
import           Data.Map                 (Map)
import Data.Function (on)
import           Data.Ord                 (Down (Down))
import qualified Data.Vector      as Vector
import qualified Data.Maybe               as Maybe

import qualified Data.PerfectHash.Hashing as Hashing
import Data.PerfectHash.Hashing (Hash, ArraySize)
import qualified Data.PerfectHash.Lookup as Lookup
import Data.PerfectHash.Types.Nonces (Nonce)
import qualified Data.PerfectHash.Types.Nonces as Nonces


data AlgorithmParams = AlgorithmParams {
    getNextNonceCandidate :: Nonce -> Nonce
  , startingNonce :: Nonce
  }


-- | NOTE: Vector may peform better for these structures, but
-- the code may not be as clean.
data LookupTable a = NewLookupTable {
    redirs :: IntMap Nonce
  , vals   :: IntMap a
  }


data SingletonBucket a = SingletonBucket Hash a
  deriving Eq


data HashBucket a = HashBucket {
    _hashVal :: Hash
  , bucketMembers :: [a]
  }

instance Eq (HashBucket a) where 
  (==) = (==) `on` (Down . length . bucketMembers)

instance Ord (HashBucket a) where 
  compare = compare `on` (Down . length . bucketMembers)


data SizedList a = SizedList [a] ArraySize
data IntMapAndSize a = IntMapAndSize (IntMap a) ArraySize


-- | slots for each bucket, with the current nonce attempt
data PlacementAttempt a = PlacementAttempt Nonce [SingletonBucket a]


emptyLookupTable :: LookupTable a
emptyLookupTable = NewLookupTable mempty mempty


-- * Functions

convertToVector
  :: (Default a)
  => LookupTable a
  -> Lookup.LookupTable a
convertToVector x = Lookup.LookupTable a1 a2
  where
    size = length $ vals x
    vectorize input = Vector.generate size $
      flip (IntMap.findWithDefault def) input

    a1 = vectorize $ redirs x
    a2 = vectorize $ vals x


-- | Computes a slot in the destination array (Data.PerfectHash.Lookup.values)
-- for every element in this multi-entry bucket, for the given nonce.
--
-- Return a Nothing for a slot if it collides.
--
-- This function is able to fail fast if one of the elements of the bucket
-- yields a collision when using the new nonce.
attemptNonceRecursive
  :: Hashing.ToHashableChunks a
  => IntMapAndSize b
  -> Nonce
  -> IntSet -- ^ occupied slots
  -> [(a, b)] -- ^ keys
  -> [Maybe Hashing.SlotIndex]
attemptNonceRecursive _ _ _ [] = []
attemptNonceRecursive
    values_and_size
    nonce
    occupied_slots
    ((current_key, _):remaining_bucket_keys) =

  if cannot_use_slot
    then pure Nothing
    else Just slot : recursive_result

  where
    IntMapAndSize values size = values_and_size
    slot = Hashing.hashToSlot nonce size current_key

    Hashing.SlotIndex slotval = slot

    -- TODO: Create a record "SlotOccupation" to encapsulate the IntSet implementation
    cannot_use_slot = IntSet.member slotval occupied_slots || IntMap.member slotval values

    recursive_result = attemptNonceRecursive
      values_and_size
      nonce
      (IntSet.insert slotval occupied_slots)
      remaining_bucket_keys


-- | Repeatedly try different values of the nonce until we find a hash function
-- that places all items in the bucket into free slots.
--
-- Increment the candidate nonce by @1@ each time.
-- Theoretically we're guaranteed to eventually find a solution.
findNonceForBucket
  :: (Hashing.ToHashableChunks a)
  => AlgorithmParams
  -> Nonce -- ^ nonce to attempt
  -> IntMapAndSize b
  -> [(a, b)] -- ^ colliding keys for this bucket
  -> PlacementAttempt (a, b)
findNonceForBucket algorithm_params nonce_attempt values_and_size bucket =

  -- This is a "lazy" (and awkward) way to specify recursion:
  -- If the result ("result_for_this_iteration") at this iteration of the recursion
  -- is not "Nothing", then, wrap it in a "PlacementAttempt" record.
  -- Otherwise, descend one layer deeper by computing "recursive_result".
  maybe
    recursive_result
    f
    maybe_result_for_this_iteration

  where
    f = PlacementAttempt nonce_attempt .
      flip (zipWith SingletonBucket) bucket . map Hashing.getIndex

    -- NOTE: attemptNonceRecursive returns a list of "Maybe SlotIndex"
    -- records. If *any* of those elements are Nothing (that is, at
    -- least one of the slots were not successfully placed), then
    -- sequenceA of that list will become Nothing.
    maybe_result_for_this_iteration = sequenceA $ attemptNonceRecursive
      values_and_size
      nonce_attempt
      mempty
      bucket

    recursive_result = findNonceForBucket
      algorithm_params
      (getNextNonceCandidate algorithm_params nonce_attempt)
      values_and_size
      bucket


-- | Searches for a nonce for this bucket, starting with the value @1@,
-- until one is found that results in no collisions for both this bucket
-- and all previously placed buckets.
handleMultiBuckets
  :: (Hashing.ToHashableChunks a)
  => AlgorithmParams
  -> ArraySize
  -> LookupTable b
  -> HashBucket (a, b)
  -> LookupTable b
handleMultiBuckets
    algorithm_params
    size
    old_lookup_table
    (HashBucket computed_hash bucket) =

  NewLookupTable new_g new_values_dict
  where
    NewLookupTable old_g old_values_dict = old_lookup_table

    sized_vals_dict = IntMapAndSize old_values_dict size

    -- This is assured to succeed; it starts with a nonce of 1
    -- but keeps incrementing it until all of the keys in this
    -- bucket are placeable.
    PlacementAttempt nonce slots_for_bucket =
      findNonceForBucket
        algorithm_params
        (startingNonce algorithm_params)
        sized_vals_dict bucket

    new_g = IntMap.insert computed_hash nonce old_g

    new_values_dict = foldr f old_values_dict slots_for_bucket

    f (SingletonBucket slot_val (_, map_val)) =
      IntMap.insert slot_val map_val


-- | This function exploits the sorted structure of the list
-- by skimming the multi-entry buckets from the front of the
-- list. Then we filter the single-entry buckets by dropping
-- the empty buckets.
findCollisionNonces
  :: (Hashing.ToHashableChunks a)
  => AlgorithmParams
  -> ArraySize
  -> SortedList (HashBucket (a, b))
  -> (LookupTable b, [SingletonBucket (a, b)])
findCollisionNonces algorithm_params size sorted_bucket_hash_tuples =

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
      (handleMultiBuckets algorithm_params size)
      emptyLookupTable
      multi_entry_buckets

    remaining_words = Maybe.mapMaybe
      convertToSingletonBucket
      single_or_fewer_buckets

    convertToSingletonBucket (HashBucket hashVal elements) =
      SingletonBucket hashVal <$> Maybe.listToMaybe elements


-- | Sort buckets by descending size
preliminaryBucketPlacement
  :: (Hashing.ToHashableChunks a)
  => SizedList (a, b)
  -> SortedList (HashBucket (a, b))
preliminaryBucketPlacement tuplified_words_dict_and_size =
  toSortedList bucket_hash_tuples
  where
    SizedList tuplified_words_dict size = tuplified_words_dict_and_size

    f = Hashing.getIndex . Hashing.hashToSlot (Nonces.Nonce 0) size . fst

    slot_key_pairs = deriveTuples f tuplified_words_dict

    bucket_hash_tuples = map (uncurry HashBucket) $
      IntMap.toList $ binTuplesBySecond slot_key_pairs


-- | Generates a minimal perfect hash for a set of key-value pairs.
--
-- The keys must be instances of 'Hashing.ToHashableChunks'.
-- The values may be of arbitrary type.
--
-- A 'Map' is required as input to guarantee that there are
-- no duplicate keys.
createMinimalPerfectHash
  :: (Hashing.ToHashableChunks k, Default v)
  => Map k v -- ^ key-value pairs
  -> Lookup.LookupTable v
     -- ^ output for use by 'LookupTable.lookup' or a custom code generator
createMinimalPerfectHash original_words_dict =
  convertToVector $ NewLookupTable final_g final_values
  where
    algorithm_params = AlgorithmParams
      (Nonces.mapNonce (+1))
      (Nonces.Nonce 1)

    tuplified_words_dict = Map.toList original_words_dict
    size = Hashing.ArraySize $ length tuplified_words_dict
    tuplified_words_dict_and_size = SizedList tuplified_words_dict size
    sorted_bucket_hash_tuples = preliminaryBucketPlacement tuplified_words_dict_and_size

    -- TODO: Rename this variable: "remaining_word_hash_tuples"
    (intermediate_lookup_table, remaining_word_hash_tuples) =
      findCollisionNonces
        algorithm_params
        size
        sorted_bucket_hash_tuples

    isUnusedSlot (Hashing.SlotIndex s) =
      not $ IntMap.member s $ vals intermediate_lookup_table

    unused_slots = filter isUnusedSlot $ Hashing.generateArrayIndices size

    zipped_remaining_with_unused_slots =
      zip remaining_word_hash_tuples unused_slots

    f1 (SingletonBucket computed_hash _, Hashing.SlotIndex free_slot_index) =
      -- Observe here that both the output and input
      -- are nonces:
      IntMap.insert computed_hash $ Nonces.Nonce $
        Hashing.getIndex $ Lookup.encodeDirectEntry $
          Nonces.Nonce free_slot_index

    final_g = foldr
      f1
      (redirs intermediate_lookup_table)
      zipped_remaining_with_unused_slots

    f2 (SingletonBucket _ (_, map_value), Hashing.SlotIndex free_slot_index) =
      IntMap.insert free_slot_index map_value

    final_values = foldr
      f2
      (vals intermediate_lookup_table)
      zipped_remaining_with_unused_slots


-- * Utility functions

-- | Place the first elements of the tuples into bins according to the second
-- element.
binTuplesBySecond
  :: (Foldable t)
  => t (a, Int)
  -> IntMap [a]
binTuplesBySecond = foldr f mempty
  where
    f = uncurry (IntMap.insertWith mappend) .
      fmap pure . swap


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
