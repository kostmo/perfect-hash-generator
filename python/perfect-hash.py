#!/usr/bin/python -tt

# Easy Perfect Minimal Hashing 
# By Steve Hanov. Released to the public domain.
#
# Based on:
# Edward A. Fox, Lenwood S. Heath, Qi Fan Chen and Amjad M. Daoud, 
# "Practical minimal perfect hash functions for large databases", CACM, 35(1):105-121
# also a good reference:
# Compress, Hash, and Displace algorithm by Djamal Belazzougui,
# Fabiano C. Botelho, and Martin Dietzfelbinger
import sys


DICTIONARY = "/usr/share/dict/words"


# Calculates a distinct hash function for a given string. Each value of the
# integer d results in a different hash value.
def hash( d, s ):

    FNV_prime = 0x01000193

    if d == 0: d = FNV_prime


    # Use the FNV-1a algorithm from http://isthe.com/chongo/tech/comp/fnv/
    for c in s:
        d = ( (d ^ ord(c)) * FNV_prime  ) & 0xffffffff;

    return d



def attempt_bucket_nonce(size, values, nonce, bucket):

    slots = []
    for bucket_val in bucket:

        slot = hash( nonce, bucket_val ) % size

        if values[slot] != None or slot in slots:
            return None
        else:
            slots.append( slot )

    return slots



def find_nonce_for_bucket(size, values, bucket):

    nonce = 1

    # Repeatedly try different values of d until we find a hash function
    # that places all items in the bucket into free slots
    while True:

        slots = attempt_bucket_nonce(size, values, nonce, bucket)

        if slots is not None:
            return (slots, nonce)
        else:
            nonce += 1



def find_collision_nonces(
        words_dict,
        size,
        values,
        sorted_bucket_hash_tuples,
    ):
    """
    NOTE: This mutates the "values" array
    """

    G = [0] * size

    for bucket_index, (bucket, computed_hash) in enumerate(sorted_bucket_hash_tuples):

        # Since the buckets have been sorted by descending size,
        # once we get to the bucket with 1 or fewer elements,
        # we know there are no more collision buckets.

        if len(bucket) <= 1:
            break
        
        (slots, d) = find_nonce_for_bucket(size, values, bucket)

        G[computed_hash] = d
        for i, bucket_val in enumerate(bucket):
            values[slots[i]] = words_dict[bucket_val]


    remaining_buckets = sorted_bucket_hash_tuples[bucket_index:]
    


    remaining_words = []

    for (bucket, computed_hash) in remaining_buckets:

        # Since the buckets have been sorted by descending size,
        # once we get to the bucket with zero elements,
        # we know there are no more buckets with content.

        if len(bucket) == 0:
            break

        remaining_words.append( (bucket[0], computed_hash) )

    return (G, remaining_words)



def preliminary_bucket_placement( words_dict ):

    size = len(words_dict)

    buckets = [ [] for i in range(size) ]

    for key in words_dict.keys():
        bucket_slot = hash(0, key) % size
        buckets[bucket_slot].append( key )

    bucket_hash_tuples = [(sorted(bucket), h) for (h, bucket) in enumerate(buckets) ]

    bucket_hash_tuples.sort( key=lambda x: (len(x[0]), x[1], x[0]), reverse=True )
    return bucket_hash_tuples



# Computes a minimal perfect hash table using the given python dictionary. It
# returns a tuple (G, V). G and V are both arrays. G contains the intermediate
# table of values needed to compute the index of the value in V. V contains the
# values of the dictionary.
def CreateMinimalPerfectHash( words_dict ):


    # Step 1: Place all of the keys into buckets

    sorted_bucket_hash_tuples = preliminary_bucket_placement(words_dict)

    size = len(words_dict)
    values = [None] * size

    # Step 2: Sort the buckets and process the ones with the most items first.
    (G, remaining_word_hash_tuples) = find_collision_nonces(
        words_dict,
        size,
        values,
        sorted_bucket_hash_tuples,
    )


    # Only buckets with 1 item remain. Process them more quickly by directly
    # placing them into a free slot. Use a negative value of d to indicate
    # this.
    freelist = []
    for i, val in enumerate(values): 
        if val is None:
            freelist.append( i )



    for ((word, computed_hash), free_slot_index) in zip(remaining_word_hash_tuples, freelist):


        # We subtract one to ensure it's negative even if the zeroeth slot was
        # used.

        G[computed_hash] = -free_slot_index - 1 

        values[free_slot_index] = words_dict[ word ]


    return (G, values)



# Look up a value in the hash table, defined by G and V.
def PerfectHashLookup( G, V, key ):

    size = len(G)

    nonce = G[hash(0, key) % size]

    # Negative index signals that we don't need extra lookup layer
    v_key = -nonce - 1 if nonce < 0 else hash(nonce, key) % size

    return V[v_key]



def demoHashingFunction( word_to_hash ):
   print 'HASH of "%s": %d' % (word_to_hash, hash( 0, word_to_hash ))



if __name__ == "__main__":

    print "Reading words"
    words_dict = {}


    with open(DICTIONARY) as fh:
        for line_index, key in enumerate( fh.readlines() ):
            words_dict[key.strip()] = line_index + 1


    print "Creating perfect hash..."
    (G, V) = CreateMinimalPerfectHash( words_dict )


    recreated_words_dict = {}

    for word, actual_line_index in words_dict.items():
        perfect_line_index = PerfectHashLookup( G, V, word )

        if actual_line_index != perfect_line_index:
            sys.exit("Error on word " + word)

    demoHashingFunction("blarg")

