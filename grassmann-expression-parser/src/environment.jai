#scope_export

// TODO: Doesn't handle changing dimensions gracefully
// If the dimension is less than the current but the symbols didn't change we don't have to do anything
// If the symbols changed then we have to regenerate the symbols
// If the dimension expands then we need to go through this whole process again because we introduced new one elements
// and everyone's index is hosed!!!

MAX_DIMENSION :: 6;
MAX_NUM_BASIS_ELEMENTS :: #run power( 2, MAX_DIMENSION );

Basis_Element :: struct {
	display_order:  int;
	grade:  		int;
	symbol: 		string;
}

Basis_Element_Index :: #type s64;

/*
	All K-Elements are built using the bit-indices of the 1-elements;
	Take G(R4) as an example:
		e1 = 1 = (1 << 1)
		e2 = 2 = (1 << 2)
		e3 = 4 = (1 << 3)
		e4 = 8 = (1 << 4)

	The 'Basis_Element_Index' represents the INTEGER value of the product of any given number of 1-elements.
	As examples:
		e1^e2 	 is equal to  3 or e1 | e2.
		e1^e3^e4 is equal to 13 or e1 | e3 | e4
		etc..
*/

K_Element :: struct {
	coefficient:   int;
	basis:         Basis_Element_Index;
}

Environment :: struct {
	dimension: int;
	one_element_symbols: [] string;
	basis_elements:      [MAX_NUM_BASIS_ELEMENTS] Basis_Element;
	n_element:           s64;
}

environment_set_dimension_and_n_element :: ( env: *Environment, dimension: int ) {
	
	assert( dimension <= MAX_DIMENSION );
	env.dimension = dimension;

	env.n_element = 0;
	for i: 0..dimension-1 {
		env.n_element |= (1 << i);
	}

	return;
}

environment_set_one_element_symbols :: ( env: *Environment, one_element_symbols: [] string ) {	
	// TODO: if they are already set then we need to free and point to the new set of symbols
	env.one_element_symbols = one_element_symbols;
	return;
}

environment_generate_basis_elements :: ( env: *Environment ) {

	// This implementation is admittedly pretty slow.
	// Building the display order of basis symbols, the bit/pop count and the generation of the basis element 
	// symbols require a lot of iteration and bit checking.
	//
	// This process shouldn't be executed very often during normal use.
	// It is typical to declare a space and just stick to it.
	//
	// Also using the bit structure of elements of grades > 1 as simple AND and ORs makes doing this worth the
	// effort, at least for now.
	
	assert( env.dimension == env.one_element_symbols.count );
	assert( env.dimension <= MAX_DIMENSION );

	Order :: struct {
		accum:  int; // n_choose_k for n-1; so each element at index i has the sum total of basis elements prior to it	
		cursor: int; // this is the cursor for a particular grade, it will iterate up to the num of basis elements for a particular grade given the dimension
	}

	basis_element_display_order: [MAX_DIMENSION] Order;

	basis_element_display_order[0].accum  = 1;
	basis_element_display_order[0].cursor = 0;
	for i: 1..env.dimension-1 {
		basis_element_display_order[i].accum  = choose( env.dimension, i ) + basis_element_display_order[i-1].accum;
		basis_element_display_order[i].cursor = 0;
	}

	total_num_basis_elements: int = power( 2, env.dimension );

	env.basis_elements[0].display_order = 0;
	env.basis_elements[0].grade  		= 0;
	env.basis_elements[0].symbol 		= "1";
	
	for i: 1..total_num_basis_elements-1 {
		
		grade := pop_count( i );

		env.basis_elements[i].display_order = basis_element_display_order[grade-1].accum + basis_element_display_order[grade].cursor;
		env.basis_elements[i].grade  		= grade;
		env.basis_elements[i].symbol 		= generate_basis_symbol( element = i, grade, env.dimension, env.one_element_symbols );

		basis_element_display_order[grade].cursor += 1;
	}
}

#scope_file

choose :: (n: int, k: int) -> (value: int) {
	// example: choose(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * choose(n-1, k-1)) / k;
}

power :: ( a: int, b: int ) -> ( c: int ) {
	
	// Implements the basic arithment expression: c = a^b

	c: int = 1;
	for i: 0..b-1 {
		c *= a;
	}
	return c;
}

pop_count :: ( x: int ) -> ( bit_count: int ) {

	// Implementation from here: https://en.wikichip.org/wiki/population_count
	bit_count: int = 0;
	while x != 0 {
		bit_count += 1;
		x = x & (x-1);
	}
	
	return bit_count;
}

generate_basis_symbol :: ( element: int, grade: int, dimension: int, one_element_symbols: [] string ) -> ( symbol: string ) {

	builder: String_Builder;
	num_processed_basis_one_elements: int = 0;

	if grade == 0 {
		// this might not be the correct play when printing one-elements
		append( *builder, "1" );
	}
	else {
		// Iterate through each bit and lookup the corresponding basis symbol supplied by the user
		// This is slow when building the entire set of basis elements for the user however this code path
		// is traversed maybe once or twice during a run of the application so willing to put up with this for now.
		// Also if the dimension (N) starts creeping up then this gets even slower but it will for sure work, which
		// was not guaranteed with the other approaches I took.
		
		for i: 0..dimension-1 {
			if element & (1 << i) {
				append( *builder, one_element_symbols[i] );
				if num_processed_basis_one_elements < grade-1 { // in an expression like e1^e2 the number of exterior products is the grade-1
					append( *builder, "^" );
				}
				num_processed_basis_one_elements += 1;
			}
		}
	}

	// TODO: Leaking memory!!!
	symbol: string = builder_to_string( *builder );
	return symbol;
}
