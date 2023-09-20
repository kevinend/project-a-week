#scope_export

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

Multi_Element :: [] K_Element;

Environment :: struct {
	dimension: int;
	one_element_symbols: [] string;
	basis_elements:      [MAX_NUM_BASIS_ELEMENTS] Basis_Element;
	num_basis_elements:  int;
	n_element:           s64;
}

environment_set_dimension_and_n_element :: ( env: *Environment, dimension: int ) {
	
	assert( dimension <= MAX_DIMENSION );
	env.dimension = dimension;

	env.n_element = 0;
	for i: 0..dimension-1 {
		env.n_element |= (1 << i);
	}

	env.num_basis_elements = power( 2, dimension );

	return;
}

environment_set_one_element_symbols :: ( env: *Environment, one_element_symbols: [] string ) {	
	// TODO: if they are already set then we need to free and point to the new set of symbols
	env.one_element_symbols = one_element_symbols;
	return;
}

environment_generate_basis_elements :: ( env: *Environment ) {
	
	// Generates the list of basis elements for the currently declared dimension.
	//
	// Since the application uses bit operations of 1-elements to build elements of higher grade
	// the order of the elements needs determined ahead of time then referenced later when building the final expression.
	//
	// We want a deterministic printing/display order of the elements.
	//
	// Example: (Not all basis elements shown)
	//  N = 3
	//  e1 	  = (1 << 1) = 1
	//  e2 	  = (1 << 2) = 2
	//  e1^e2 =  1  | 2  = 3 
    //  e3 	  = (1 << 3) = 4
    //
	//
	// If we have the basis elements e1, e2, e3 and e1^e2 without a 'display-order' field an expression
	// could print interleaved grades like e1 + e2 + e1^e2 + e3, which is not ideal.
	//
	// The display order field allows us to print 1-elements, followed by 2-elements, followed by ...
	// all the way up to the N-element.

	assert( env.dimension == env.one_element_symbols.count );
	assert( env.dimension <= MAX_DIMENSION );

	accum_elements_per_grade: [MAX_DIMENSION] int; // accumulated basis elements; number of elements at the grade specified + the num elements in any prior grade
	cursors: 				  [MAX_DIMENSION] int;

	accum_elements_per_grade[0] = 1;
	cursors[0] = 0;

	for i: 1..env.dimension-1 {
		accum_elements_per_grade[i] = choose( env.dimension, i ) + accum_elements_per_grade[i-1];
		cursors[i] = 0;
	}

	env.basis_elements[0].display_order = 0;
	env.basis_elements[0].grade         = 0; 
	env.basis_elements[0].symbol        = ""; // if we print the basis elements to the user make sure to replace this!

	for i: 1..env.num_basis_elements-1 {
		
		grade := pop_count( i );

		env.basis_elements[i].display_order = accum_elements_per_grade[grade-1] + cursors[grade];
		env.basis_elements[i].grade  		= grade;
		env.basis_elements[i].symbol 		= generate_basis_symbol( element = i, grade, env.dimension, env.one_element_symbols );

		cursors[grade] += 1;
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
