{-# OPTIONS_HADDOCK prune #-}

-- | Constructs a minimal perfect hash from a map of key-value pairs.
--
-- = Overview of algorithm
-- A two-input hash function @F(nonce, key)@ is used.
--
-- 1. Keys are hashed into buckets for the first round with a nonce of @0@.
-- 1. Iterating over each bucket of size @>= 2@ in order of decreasing size, keep
--    testing different nonce values until all members
--    of the bucket fall into open slots in the final array.
--    When a successful nonce is found, write it to the \"intermediate\" array
--    at the bucket's position.
-- 1. For each bucket of size @1@, select an arbitrary open slot in the final
--    array, and write the slot's
--    index (after negation and subtracting @1@) to the intermediate array.
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
  , createMinimalPerfectHash'
  , createMinimalPerfectHashWithKeys
  ) where

import Control.Arrow (first)
import Data.Tuple (swap)
import           Control.Monad            (join, guard)
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
import Data.PerfectHash.Hashing (Hash32, ArraySize)
import qualified Data.PerfectHash.Lookup as Lookup
import Data.PerfectHash.Types.Nonces (Nonce)
import qualified Data.PerfectHash.Types.Nonces as Nonces


data AlgorithmParams a = AlgorithmParams {
    nonceParams :: NonceFindingParams
  , hashFunction :: Hashing.HashFunction a Hash32
  }


data NonceFindingParams = NonceFindingParams {
    getNextNonceCandidate :: Nonce -> Nonce
  , startingNonce :: Nonce
  }


data NonceOrDirect =
    WrappedNonce Nonce
  | DirectEntry Hashing.SlotIndex
  | UnusedSlot


-- | NOTE: Vector might perform better for these structures, but
-- the code may not be as clean.
data LookupTable a = NewLookupTable {
    nonces :: IntMap NonceOrDirect
  , vals   :: IntMap a
  }


data SingletonBucket a = SingletonBucket Hash32 a
  deriving Eq


data HashBucket a = HashBucket {
    _hashVal :: Hash32
  , bucketMembers :: [a]
  }

instance Eq (HashBucket a) where 
  (==) = (==) `on` (Down . length . bucketMembers)

instance Ord (HashBucket a) where 
  compare = compare `on` (Down . length . bucketMembers)


data SizedList a = SizedList [a] ArraySize

data IntMapAndSize a = IntMapAndSize (IntMap a) ArraySize


data SlotBucket a = SlotBucket Hashing.SlotIndex a


-- | slots for each bucket with the current nonce attempt
data PlacementAttempt a = PlacementAttempt Nonce [SlotBucket a]


data PartialSolution a b = PartialSolution (LookupTable b) [SingletonBucket (a, b)]


-- * Constants

emptyLookupTable :: LookupTable a
emptyLookupTable = NewLookupTable mempty mempty


defaultAlgorithmParams
  :: Hashing.ToOctets a
  => AlgorithmParams a
defaultAlgorithmParams = AlgorithmParams
  (NonceFindingParams (Nonces.mapNonce (+1)) (Nonces.Nonce 1))
  Hashing.modernHash


-- * Functions

toRedirector :: NonceOrDirect -> Int
toRedirector (WrappedNonce (Nonces.Nonce x)) = x
toRedirector (DirectEntry free_slot_index) =
  Lookup.encodeDirectEntry free_slot_index
toRedirector UnusedSlot = 0


convertToVector
  :: LookupTable a
  -> Lookup.LookupTable a
convertToVector x = Lookup.LookupTable a1 a2
  where
    size = length $ vals x
    
    -- The keys of the "nonces" map may not be consecutive integers,
    -- since by virtue of the algorithm some nonces are used for
    -- multiple keys.
    vectorizeNonces input = Vector.generate size $
      toRedirector . flip (IntMap.findWithDefault UnusedSlot) input

    a1 = vectorizeNonces $ nonces x

    -- The keys of the "vals" map are consecutive integers
    -- starting with 0.
    a2 = Vector.fromList $ map snd $ IntMap.toAscList $ vals x



data NonceAttemptAccumulator = NonceAttemptAccumulator IntSet [Hashing.SlotIndex]


-- | Computes a slot in the destination array (Data.PerfectHash.Lookup.values)
-- for every element in this multi-entry bucket, for the given nonce.
--
-- Return Nothing if a slot collides.
--
-- This function is able to fail-fast if one of the elements of the bucket
-- yields a collision when using the new nonce.
attemptNonce
  :: Hashing.ToOctets a
  => AlgorithmParams a
  -> IntMapAndSize b
  -> Nonce
  -> [a] -- ^ keys
  -> Maybe [Hashing.SlotIndex]
attemptNonce
    algorithm_params
    values_and_size
    nonce
    remaining_bucket_keys = do

    NonceAttemptAccumulator _ vals <- maybe_fold_result
    return vals

  where
    maybe_fold_result = foldr myCallback (Just $ NonceAttemptAccumulator mempty []) remaining_bucket_keys

    IntMapAndSize values size = values_and_size

    myCallback current_key maybe_prev_accumulator = do
      NonceAttemptAccumulator occupied_slots_for_this_nonce_attempt placed_slots <- maybe_prev_accumulator
      guard $ IntSet.notMember slotval occupied_slots_for_this_nonce_attempt
      guard $ IntMap.notMember slotval values
      return $ NonceAttemptAccumulator
        (IntSet.insert slotval occupied_slots_for_this_nonce_attempt)
        (slot:placed_slots)

      where
        slot = Hashing.hashToSlot
          (hashFunction algorithm_params)
          (Just nonce)
          size
          current_key

        Hashing.SlotIndex slotval = slot


-- | Repeatedly try different values of the nonce until we find a hash function
-- that places all items in the bucket into free slots.
--
-- Increment the candidate nonce by @1@ each time.
-- Theoretically we're guaranteed to eventually find a solution.
findNonceForBucketRecursive
  :: (Hashing.ToOctets a)
  => AlgorithmParams a
  -> Nonce -- ^ nonce to attempt
  -> IntMapAndSize b -- ^ previously placed buckets
  -> [(a, b)] -- ^ colliding keys for this bucket
  -> PlacementAttempt (a, b)
findNonceForBucketRecursive algorithm_params nonce_attempt previous_placements bucket =

  -- This is a "lazy" (and awkward) way to specify recursion:
  -- If the result ("result_for_this_iteration") at this iteration of the recursion
  -- is not "Nothing", then, wrap it in a "PlacementAttempt" record.
  -- Otherwise, descend one layer deeper by computing "recursive_result".
  maybe
    recursive_result
    wrapSlotIndicesAsAttempt
    maybe_final_result

  where
    wrapSlotIndicesAsAttempt = PlacementAttempt nonce_attempt .
      flip (zipWith SlotBucket) bucket

    maybe_final_result = attemptNonce
      algorithm_params
      previous_placements
      nonce_attempt
      (map fst bucket)

    recursive_result = findNonceForBucketRecursive
      algorithm_params
      (getNextNonceCandidate (nonceParams algorithm_params) nonce_attempt)
      previous_placements
      bucket


-- | Searches for a nonce for this bucket, starting with the value @1@,
-- until one is found that results in no collisions for both this bucket
-- and all previously placed buckets.
processMultiEntryBuckets
  :: (Hashing.ToOctets a)
  => AlgorithmParams a
  -> ArraySize
  -> LookupTable b
  -> HashBucket (a, b)
  -> LookupTable b
processMultiEntryBuckets
    algorithm_params
    size
    previously_placed_buckets
    (HashBucket computed_hash bucket_members) =

  NewLookupTable new_nonces new_values_dict
  where
    NewLookupTable old_nonces old_values_dict = previously_placed_buckets

    previous_placements = IntMapAndSize old_values_dict size

    -- This is assured to succeed; it starts with a nonce of 1
    -- but keeps incrementing it until all of the keys in this
    -- bucket are placeable.
    PlacementAttempt nonce slots_for_bucket =
      findNonceForBucketRecursive
        algorithm_params
        (startingNonce $ nonceParams algorithm_params)
        previous_placements
        bucket_members

    new_nonces = IntMap.insert
      (fromIntegral $ Hashing.getHash computed_hash)
      (WrappedNonce nonce)
      old_nonces

    new_values_dict = foldr place_values old_values_dict slots_for_bucket

    place_values (SlotBucket slot_val (_, value)) =
      IntMap.insert (Hashing.getIndex slot_val) value


-- | This function exploits the sorted structure of the list
-- by skimming the multi-entry buckets from the front of the
-- list. Then we filter the single-entry buckets by dropping
-- the empty buckets.
--
-- The partial solution produced by this function entails
-- all of the colliding nonces as fully placed.
handleCollidingNonces
  :: (Hashing.ToOctets a)
  => AlgorithmParams a
  -> ArraySize
  -> SortedList (HashBucket (a, b))
  -> PartialSolution a b
handleCollidingNonces algorithm_params size sorted_bucket_hash_tuples =

  PartialSolution lookup_table non_colliding_buckets
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
      (processMultiEntryBuckets algorithm_params size)
      emptyLookupTable
      multi_entry_buckets

    non_colliding_buckets = Maybe.mapMaybe
      convertToSingletonBucket
      single_or_fewer_buckets

    convertToSingletonBucket (HashBucket hashVal elements) =
      SingletonBucket hashVal <$> Maybe.listToMaybe elements


-- | Hash the keys into buckets and sort them by descending size
preliminaryBucketPlacement
  :: (Hashing.ToOctets a)
  => AlgorithmParams a
  -> SizedList (a, b)
  -> SortedList (HashBucket (a, b))
preliminaryBucketPlacement algo_params sized_list =
  toSortedList bucket_hash_tuples
  where
    SizedList tuplified_words_dict size = sized_list

    h = Hashing.hashToSlot (hashFunction algo_params) Nothing size
    f = Hashing.getIndex . h . fst

    slot_key_pairs = deriveTuples f tuplified_words_dict

    bucket_hash_tuples = map (uncurry HashBucket . first (Hashing.Hash . fromIntegral)) $
      IntMap.toList $ binTuplesBySecond slot_key_pairs


-- | Arbitrarily pair the non-colliding buckets with free slots.
--
-- At this point, all of the "colliding" hashes have been resolved
-- to their own slots, so we just take the leftovers.
assignDirectSlots
  :: ArraySize
  -> PartialSolution a b
  -> LookupTable b
assignDirectSlots size (PartialSolution intermediate_lookup_table non_colliding_buckets) =
  NewLookupTable final_nonces final_values
  where
    isUnusedSlot (Hashing.SlotIndex s) =
      IntMap.notMember s $ vals intermediate_lookup_table

    unused_slots = filter isUnusedSlot $ Hashing.generateArrayIndices size

    zipped_remaining_with_unused_slots =
      zip non_colliding_buckets unused_slots

    insertDirectEntry (SingletonBucket computed_hash _, free_slot_index) =
      IntMap.insert (fromIntegral $ Hashing.getHash computed_hash) $ DirectEntry free_slot_index

    final_nonces = foldr
      insertDirectEntry
      (nonces intermediate_lookup_table)
      zipped_remaining_with_unused_slots

    f2 (SingletonBucket _ (_, map_value), Hashing.SlotIndex free_slot_index) =
      IntMap.insert free_slot_index map_value

    final_values = foldr
      f2
      (vals intermediate_lookup_table)
      zipped_remaining_with_unused_slots


-- | Generates a minimal perfect hash for a set of key-value pairs.
--
-- The keys must be instances of 'Hashing.ToOctets'.
-- The values may be of arbitrary type.
--
-- A 'Map' is required as input to guarantee that there are
-- no duplicate keys.
createMinimalPerfectHash
  :: Hashing.ToOctets k
  => Map k v -- ^ key-value pairs
  -> Lookup.LookupTable v
     -- ^ output for use by 'Lookup.lookup' or a custom code generator
createMinimalPerfectHash =
  createMinimalPerfectHash' defaultAlgorithmParams


-- | Parameterized variant of 'createMinimalPerfectHash'
createMinimalPerfectHash'
  :: Hashing.ToOctets k
  => AlgorithmParams k
  -> Map k v -- ^ key-value pairs
  -> Lookup.LookupTable v
     -- ^ output for use by 'Lookup.lookup' or a custom code generator
createMinimalPerfectHash' algorithm_params original_words_dict =
  convertToVector final_solution
  where
    tuplified_words_dict = Map.toList original_words_dict
    size = Hashing.ArraySize $ length tuplified_words_dict
    sized_list = SizedList tuplified_words_dict size

    sorted_bucket_hash_tuples = preliminaryBucketPlacement
      algorithm_params
      sized_list

    partial_solution = handleCollidingNonces
      algorithm_params
      size
      sorted_bucket_hash_tuples

    final_solution = assignDirectSlots size partial_solution


-- | Stores the keys alongside the values so that lookups using
-- invalid keys can be detected.
createMinimalPerfectHashWithKeys
  :: Hashing.ToOctets k
  => Map k v -- ^ key-value pairs
  -> Lookup.LookupTable (k, v)
     -- ^ output for use by 'Lookup.lookup' or a custom code generator
createMinimalPerfectHashWithKeys =
  createMinimalPerfectHash . Map.mapWithKey (,)


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
