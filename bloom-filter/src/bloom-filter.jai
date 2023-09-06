/*
	The bloom filter is a data structure that answers the question
	"Is an element in a set?". It can generate false positives, the element
	is in the set when it isn't but it can't generate false negatives. That is
	to say that it won't report an element not being in a set when it is.

	So the primary utility is you can know for certain whether an element has not
	been added as part of the set. There aren't immediate use-cases that come to
	mind for problems I am interested in however if you wanted to show users
	unique content (stuff they haven't seen before) I could see the utility
	of this data structure.

	It is also very memory efficient, just using a bit array of m-bits and a
	couple of hash functions. No need to store the element anywhere like in
	a hash table or deal with collisions (probing or chaining).

	It pays a cost with the number of hash functions required to generate the bit indices
	though. If you have k-hash functions you have to compute k-hashes and then check
	the indices. Generating hash functions that provide proper distribution is a non-trivial
	task. May leave that up to the internet for this one.
*/

#scope_export

// this data structure uses a bit array, which i don't have right now. We can use a simple bit-field but maybe we implement the bit array

// i wonder if instead of a struct we should just do the strict type definition?? it is an alias for an array

Bit_Array :: struct {
	// each element of the array is 64-bits 
	bits: [] s64;
}

Bloom_Filter :: struct {
	bits: Bit_Array;
}

init_bit_array :: ( bit_array: *Bit_Array, bit_count: s64 ) {

	assert( bit_count > 0 );

	num_elements: s64 = bit_count / size_of(s64);

	bit_array.bits.data  = alloc( num_elements * size_of(s64) );
	bit_array.bits.count = num_elements; 

	return;
}

set_bit :: ( bit_array: *Bit_Array, bit_to_set: s64 ) -> ( success: bool ) {

	// determine which element in the array and bit within that element needs set
	index: s64 = bit_to_set / size_of(s64);
	bit:   s64 = bit_to_set % size_of(s64);

	assert( index < bit_array.bits.count );

	print( "Index == %, Bit == %\n", index, bit );

	bit_array.bits[index] = (1 << bit);

	return true;
}

main :: () {

	bit_array: Bit_Array;
	init_bit_array( *bit_array, bit_count = 16 );

	set_bit( *bit_array, 0 );
	set_bit( *bit_array, 1 );
	set_bit( *bit_array, 7 );
	set_bit( *bit_array, 8 );

	print( "Bit array.count == %\n", bit_array.bits.count );

	s: s64 = 10;
	return;
}

#scope_file
#import "Basic";
