#scope_export

// TODO: Doesn't handle changing dimensions gracefully
// If the dimension is less than the current but the symbols didn't change we don't have to do anything
// If the symbols changed then we have to regenerate the symbols
// If the dimension expands then we need to go through this whole process again because we introduced new one elements
// and everyone's index is hosed!!!

MAX_DIMENSION :: 6;

Basis_Element :: struct {
	grade: int;
	symbol: string;
}

Environment :: struct {
	dimension: int;
	one_element_symbols: [] string;
	basis_elements:      [64] Basis_Element; // based on the MAX_DIMENSION
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
	// Both the bit/pop count and the generation of the basis element symbols require a lot of
	// iteration and bit checking.
	//
	// This process shouldn't be executed very often during normal use.
	// It is typical to declare a space and just stick to it for the use of the app.
	
	assert( env.dimension == env.one_element_symbols.count );
	
	num_basis_elements: int = power( 2, env.dimension );

	grade:  int;
	symbol: string;

	for i: 0..num_basis_elements-1 {
		grade  = pop_count( i );
		symbol = generate_basis_symbol( element = i, grade, env.dimension, env.one_element_symbols );

		env.basis_elements[i].grade  = grade;
		env.basis_elements[i].symbol = symbol;
	}
}

#scope_file

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
