{-# OPTIONS_HADDOCK prune #-}

-- | Constructs a minimal perfect hash.
--
-- Implementation was transliterated from Python on
-- <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's Blog>
-- and then refactored.
module Data.PerfectHash.Construction (
    createMinimalPerfectHash
  , Defaultable
  ) where

import           Control.Arrow            (second)
import           Control.Monad            (join)
import           Data.Foldable            (foldl')
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.List                (sortOn)
import           Data.Ord                 (Down (Down))
import qualified Data.Vector.Unboxed      as Vector

import qualified Data.PerfectHash.Hashing as Hashing
import qualified Data.PerfectHash.Lookup  as Lookup


-- | NOTE: Vector may peform better for these structures, but
-- the code may not be as clean.
data LookupTable a = NewLookupTable {
    redirs :: HashMap Int Int
  , vals   :: HashMap Int a
  }


emptyLookupTable :: LookupTable a
emptyLookupTable = NewLookupTable HashMap.empty HashMap.empty


class Defaultable a where
  getDefault :: a

instance Defaultable Int where
  getDefault = 0


data HashMapAndSize a b = HashMapAndSize (HashMap a b) Int


convertToVector :: (Vector.Unbox a, Defaultable a) => LookupTable a -> Lookup.LookupTable a
convertToVector x = Lookup.LookupTable a1 a2
  where
    size = length $ vals x
    a1 = Vector.generate size (\z -> HashMap.lookupDefault 0 z $ redirs x)
    a2 = Vector.generate size (\z -> HashMap.lookupDefault getDefault z $ vals x)


-- | Computes a slot in the destination array (Data.PerfectHash.Lookup.values)
-- for every element in this multi-entry bucket, for the given nonce.
--
-- Return a Nothing for a slot if it collides.
attemptNonceRecursive :: Hashing.ToHashableChunks a =>
     HashMapAndSize Int b
  -> Int -- ^ nonce
  -> IntSet -- ^ occupied slots
  -> [a] -- ^ keys
  -> [Maybe Int]
attemptNonceRecursive _ _ _ [] = []
attemptNonceRecursive values_and_size nonce occupied_slots (current_key:remaining_bucket_keys) =

  if cannot_use_slot
    then pure Nothing
    else Just slot : recursive_result

  where
    HashMapAndSize values size = values_and_size
    slot = Hashing.hashToSlot nonce current_key size

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
findNonceForBucket :: Hashing.ToHashableChunks a =>
     Int -- ^ nonce to attempt
  -> HashMapAndSize Int b
  -> [a] -- ^ colliding keys for this bucket
  -> ([Int], Int)
findNonceForBucket nonce_attempt values_and_size bucket =

  maybe recursive_result (\x -> (x, nonce_attempt)) maybe_attempt_result
  where
    recursive_result = findNonceForBucket (nonce_attempt + 1) values_and_size bucket
    maybe_attempt_result = sequenceA $ attemptNonceRecursive
      values_and_size
      nonce_attempt
      mempty
      bucket


-- | Searches for a nonce for this bucket, starting with the value @1@,
-- until one is found that results in no collisions for both this bucket
-- and all previous buckets.
handleMultiBuckets :: (Hashing.ToHashableChunks a, Eq a, Hashable a) =>
     HashMapAndSize a b
  -> LookupTable b
  -> (Int, [a])
  -> LookupTable b
handleMultiBuckets sized_words_dict old_lookup_table (computed_hash, bucket) =
  NewLookupTable new_g new_values
  where
    HashMapAndSize words_dict size = sized_words_dict

    sized_vals_dict = HashMapAndSize (vals old_lookup_table) size
    (slots, nonce) = findNonceForBucket 1 sized_vals_dict bucket

    new_g = HashMap.insert computed_hash nonce (redirs old_lookup_table)
    new_values = foldr fold_func (vals old_lookup_table) $ zip [0..] bucket

    fold_func (i, bucket_val) = HashMap.insert (slots !! i) $
      HashMap.lookupDefault (error "not found") bucket_val words_dict


-- | This function exploits the sorted structure of the list twice,
-- first by skimming the multi-entry buckets, then by skimming
-- the single-entry buckets and dropping the empty buckets.
findCollisionNonces :: (Hashing.ToHashableChunks a, Eq a, Hashable a) =>
     HashMapAndSize a b
  -> [(Int, [a])]
  -> (LookupTable b, [(Int, a)])
findCollisionNonces sized_words_dict sorted_bucket_hash_tuples =

  (lookup_table, remaining_words)
  where

    -- Since the buckets have been sorted by descending size,
    -- once we get to the bucket with 1 or fewer elements,
    -- we know there are no more collision buckets.
    (multi_entry_buckets, single_or_fewer_buckets) = span ((> 1) . length . snd) sorted_bucket_hash_tuples

    -- XXX Using 'foldl' rather than 'foldr' is crucial here, given the order
    -- of the buckets. 'foldr' would actually try to place the smallest buckets
    -- first, making it improbable that the large buckets will be placeable,
    -- and potentially resulting in an infinite loop.
    lookup_table = foldl' (handleMultiBuckets sized_words_dict) emptyLookupTable multi_entry_buckets

    single_entry_buckets = takeWhile (not . null . snd) single_or_fewer_buckets
    remaining_words = map (second head) single_entry_buckets


-- | Sort buckets by descending size
preliminaryBucketPlacement :: (Hashing.ToHashableChunks a, Eq a, Hashable a) =>
     HashMap a b
  -> [(Int, [a])]
preliminaryBucketPlacement words_dict =
  sortOn (Down . length . snd) bucket_hash_tuples
  where
    size = HashMap.size words_dict
    slot_key_pairs = deriveTuples (\k -> Hashing.hashToSlot 0 k size) $ HashMap.keys words_dict

    bucket_hash_tuples = HashMap.toList $ binTuplesBySecond slot_key_pairs


-- | Generates a minimal perfect hash for a set of key-value pairs.
--
-- The keys must be 'Foldable's of 'ToNumeric' instances in order to be hashable.
-- The values may be of arbitrary type.
--
-- /__N.b.__/ It is assumed that the input tuples list has no duplicate keys.
createMinimalPerfectHash :: (Vector.Unbox b, Defaultable b, Hashing.ToHashableChunks a, Eq a, Hashable a) =>
     [(a, b)] -- ^ key-value pairs
  -> Lookup.LookupTable b
createMinimalPerfectHash tuples =
  convertToVector $ NewLookupTable final_g final_values
  where
    words_dict = HashMap.fromList tuples
    size = HashMap.size words_dict

    sorted_bucket_hash_tuples = preliminaryBucketPlacement words_dict

    (intermediate_lookup_table, remaining_word_hash_tuples) = findCollisionNonces
      (HashMapAndSize words_dict size)
      sorted_bucket_hash_tuples

    unused_slots = filter (not . (`HashMap.member` vals intermediate_lookup_table)) [0..(size - 1)]

    zipped_remaining_with_unused_slots = zip remaining_word_hash_tuples unused_slots

    -- We subtract one to ensure it's negative even if the zeroeth slot was used.
    f1 ((computed_hash, _), free_slot_index) = HashMap.insert computed_hash $ Lookup.encodeDirectEntry free_slot_index
    final_g = foldr f1 (redirs intermediate_lookup_table) zipped_remaining_with_unused_slots

    f2 ((_, word), free_slot_index) = HashMap.insert free_slot_index $
      HashMap.lookupDefault (error "Impossible!") word words_dict

    final_values = foldr f2 (vals intermediate_lookup_table) zipped_remaining_with_unused_slots


-- * Utilities

-- | Place the first elements of the tuples into bins according to the second
-- element.
binTuplesBySecond :: (Eq b, Hashable b) => [(a, b)] -> HashMap.HashMap b [a]
binTuplesBySecond = foldr f HashMap.empty
  where
    f tuple = HashMap.insertWith (++) (snd tuple) [fst tuple]


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
