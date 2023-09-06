The files in this directory implement a data structure known a a bloom filter.
The primary benefit of this data structure is to definitively whether an element is is not part of a set.

According to wikipedia querying the bloom filter will return either 'possibly in set' or
'definitely not in the set'.

It accomplishes this using multiple hash functions and a bit array. The bit array is some m-bits long
and starts with all bits set to zero. Then an element of a set is passed through a collection of hash functions,
say h1, h2, h3. The output of each hash function is a bit index that is set to one.

Then you can query if an element is part of the bloom filter by running it through each hash function and
checking the bit values. If ANY of the bits are zero then you know that the element hasn't been added to the set
otherwise at least one of the bit values would have been set. If ANY or ALL the bits are set to 1 then you know
that either the element is in the set or some of the bits were set during the insertion of another element.

So again it can only tell you for certain that you haven't seen the element before.

I don't have a particular use-case for one of these data structures but have read about them a handful of times
and never implemented one. So here goes nothing!
