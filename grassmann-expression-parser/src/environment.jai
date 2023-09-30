#scope_export

/*
	A k-element is an extension of the Grassmann idea of a k-vector which allows non-vector
	basis elements such as the origin in a point space (rather than a pure vector space).

	All K-Elements are built using the 1-elements which are currently encoded as BIT FIELDS.
	Take G(R4) as an example:
		e1 = 1 = (1 << 1)
		e2 = 2 = (1 << 2)
		e3 = 4 = (1 << 3)
		e4 = 8 = (1 << 4)

	The 'Basis_Element_Index' represents the INTEGER value a product of any given number of 1-elements.
	As examples:
	    e1       is equal to  1 or  0 | e1;
		e1^e2 	 is equal to  3 or e1 | e2.
		e1^e3^e4 is equal to 13 or e1 | e3 | e4
		etc..

	In Grassmann Algebra, a sum of elements of the same grade is known as a k-vector or k-element depending
	on the space.

	The sum of k-vectors of differing grades is known as a multivector, by extension we can have
	multielements. A multielement is a sum of k-elements of differing grades. For example e1 + e1^e2.

	In this application, a 'multielement' order is defined. That is, the basis elements composing a multielement
	are ordered such that the final result of an expression is a multielement with this order imposed.
	
	The order is from the lowest graded elements to the highest graded elements for the declared dimension with
	the additional restriction that the order of the basis one elements supplied by the user or defaulted on program
	start impose the order of the basis elements of the same grade.

	As an example in G(R4) with 1-elements [e1,e2,e3,e4], a multielement with non-zero scalar values for each basis element would look like the following.
		Ex. Multielement = e1 + e2 + e3 + e4 + e1^e2 + e1^e3 + e1^e4 + e2^e3 + e2^e4 + e3^e4 + e1^e2^e3 + e1^e2^e4 + e1^e3^e4 + e2^e3^e4 + e1^e1^e3^e4

	If terms are missing from the multielement (zero scalar values) then the final expression will be a subset of the above but still in the same order.
		Ex. Multielement = e1 + e4 + e1^e3 + e1^e2^e3^e4

	Unfortunately, the ordering is not derived directly from the 'Basis_Element_Index' used to represent each individual k-element in the space.
	The environment builds a mapping table from the 'Basis_Element_Index' to the 'Multi_Element_Index' and is used during evaluation to put elements in the correct
	order.
*/

Basis_Element_Index :: #type s64;
Multi_Element_Index :: #type s64;

Basis_Element :: struct {
	grade:  int;
	symbol: string;
	basis:  Basis_Element_Index;
}

K_Element :: struct {
	coefficient:   int;
	union {
		basis: Basis_Element_Index;    // used for tree node/grassmann product operations
		basis_element: *Basis_Element; // used in the multielement, points back to symbol for particular basis
	}
}

Multi_Element :: #type [] K_Element;

Environment :: struct {
	
	MAX_DIMENSION :: 6;
	MAX_NUM_BASIS_ELEMENTS :: #run power( 2, MAX_DIMENSION );

	dimension: int;
	n_element:           s64;
	num_basis_elements:  int;

	basis_index_to_multielement_index: [MAX_NUM_BASIS_ELEMENTS] Multi_Element_Index;

	basis_symbol_buffer: 	  *u8;
	basis_symbol_buffer_size: int;

	basis_elements: [] Basis_Element; // basis elements are stored in 'multielement' order

	one_element_symbols: [] string; 
	one_element_symbol_length: int;
}

environment_update :: ( env: *Environment, dimension: int, one_element_symbols: [] string ) {

	// update the dimension of the underlying space
	{
		assert( dimension <= Environment.MAX_DIMENSION );
		env.dimension = dimension;
	};

	// generate the n element for the space
	{
		env.n_element = 0;
		for i: 0..env.dimension-1 {
			env.n_element |= (1 << i);	
		}
	};

	// set the number of basis elements for the given dimension
	{
		env.num_basis_elements = power( 2, env.dimension );
	};

	// set the one element symbol length
	{
		// all one element symbols must be the same length!
		env.one_element_symbol_length = one_element_symbols[0].count;
	}

	// generate the basis elements from the space using the list of user-supplied one element symbols
	{
		/*
			This process is a little involved, but I wanted to maintain a consistent order for the final expression's multielement.
			Weights are assigned to the basis one elements. A basis element's weight is the sumof the weights of all basis one elements contained in the element.

			For example, given the set of basis one elements [e1,e2,e3,e4] (with dimension 4) the output order of basis elements should be the following.
				Multielement = e1 + e2 + e3 + e4 + e1^e2 + e1^e3 + e1^e4 + e2^e3 + e2^e4 + e3^e4 + e1^e2^e3 + e1^e2^e4 + e1^e3^e4 + e2^e3^e4 + e1^e1^e3^e4

			We give a higher weight to the symbols we want to have precedence, so in this case e1 has a higher weight than e2 so it's terms will be ordered first.

			You can't handle this lexigraphically since e1^e2^e3 would come before e2^3 which is not what we want.

			There is one other wrinkle to this process. The basis one elements are bit-fields which eases the calculation of exterior, regressive, and interior
			products along with left and right complements. However, the bit-field values do not play nice with this intended output order.

			So to mitigate that we create a mapping table. So that you can take a basis one element like e4, with value 8 (2^3) and determine that in the output
			order it should be at index 4.
                         		 					 1 e1 e2  e1^e2 e3 ...             e4
				basis_index_to_multielement_index: 	[0, 1, 2,   5,   3,   6,  8,  11,  4...]

						  		  1 e1 e2 e3 e4 e1e2
				multielement: 	[ 0  1  2  3  4  5  . .. ]

			There may be a simpler way to accomplish this since it does require quite a few allocations and lots of loops but couldn't think of any other approach
			so oh whale for now!
		*/

		

		one_element_weights := NewArray( count = one_element_symbols.count, T = int, allocator = temp );

		// Give the highest weight to to the first symbol, then next highest to the second symbol, etc...
		for i: 0..one_element_weights.count-1 {
			one_element_weights[i] = (1 << (one_element_weights.count-(i+1))); 
		}

		basis_element_weights := NewArray( count = env.num_basis_elements, T = int, allocator = temp );

		builder := NewArray( count = env.num_basis_elements, T = Basis_Element_Builder, allocator = temp );
		builder[0].weight = 0;
		builder[0].grade  = 0;
		builder[0].basis  = 0;

		for i: 0..env.num_basis_elements-1 {
			grade  := pop_count( i );
			weight := calculate_basis_element_weight( i, dimension, one_element_weights );

			builder[i].weight = weight;
			builder[i].grade  = grade;
			builder[i].basis  = i;
		}

		sort_by_grade_and_weight( builder );

		// PERMANENT ALLOCATIONS:
		// use the builder to update the mapping table from the basis element's index into it's location in the final multielement array
		for i: 0..env.num_basis_elements-1 {
			basis := builder[i].basis;
			env.basis_index_to_multielement_index[basis] = i;
		}

		// allocate space to store all basis element symbols for the space of dimension N
		env.basis_symbol_buffer_size = 0;
		for i: 1..env.dimension {
			
			grade := i;

			num_basis_elements_per_grade := choose( env.dimension, grade );
			symbol_length := (grade * env.one_element_symbol_length) + (grade-1); // grade-1 to account for the number of ^ (wedge) symbols

			env.basis_symbol_buffer_size += num_basis_elements_per_grade * symbol_length;
		}

		env.basis_symbol_buffer = alloc( env.basis_symbol_buffer_size * size_of(u8) );
		assert( env.basis_symbol_buffer != null );

		// create a new basis elements table that stores the grade and a reference to the basis element's symbol 
		if env.basis_elements.data != null {
			free( env.basis_elements.data );
		}
		env.basis_elements = NewArray( count = env.num_basis_elements, Basis_Element );

		cursor := env.basis_symbol_buffer;
		for i: 1..env.num_basis_elements-1 {
			symbol := generate_basis_symbol( element = builder[i].basis, grade = builder[i].grade, one_element_symbols, cursor );
			env.basis_elements[i].symbol = symbol;
			env.basis_elements[i].grade  = builder[i].grade;
			env.basis_elements[i].basis  = builder[i].basis;

			cursor += symbol.count;
		}

		// set the basis one element symbols for the environment to be used by the lexer
		// not sure how to do this yet, man this is messy

		// i'd like to point to just the buffer for the things
		env.one_element_symbols = NewArray( count = env.dimension, string );
		for i: 0..env.dimension-1 {
			env.one_element_symbols[i] = env.basis_elements[1+i].symbol;
		}

		print( "One element symbols\n" );
	};

	return;	
}

calculate_basis_element_weight :: ( element: s64, dimension: int, one_element_weights: [] int ) -> ( weight: int ) {

	weight: int = 0;
	for i: 0..dimension-1 {
		if element & (1 << i) {
			weight += one_element_weights[i];
		}
	}
	return weight;	
}

sort_by_grade_and_weight :: ( list: [] Basis_Element_Builder ) {
	// bubble sort
	for _: 0..list.count-1 {
		for i: 0..list.count-2 {
			if list[i+1].grade < list[i].grade {
				temp     := list[i];
				list[i]   = list[i+1];
				list[i+1] = temp;
			}
			else
			if list[i+1].grade == list[i].grade && list[i+1].weight > list[i].weight {
				temp     := list[i];
				list[i]   = list[i+1];
				list[i+1] = temp;
			}
		}
	}

	return;	
}

#scope_file

Basis_Element_Builder :: struct {
	weight: int;
	grade:  int;
	basis:  Basis_Element_Index;
}

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

generate_basis_symbol :: ( element: int, grade: int, one_element_symbols: [] string, cursor: *u8 ) -> ( symbol: string ) {

	symbol: string;
	
	wedge := "^";
	symbol.data = cursor;
	if grade == 0 {
		// this might not be the correct play when printing one-elements
		s := "1";
		memcpy( cursor, s.data, s.count );
		symbol.count = s.count;
	}
	else {
		// Iterate through each bit and lookup the corresponding basis symbol supplied by the user
		// This is slow when building the entire set of basis elements for the user however this code path
		// is traversed maybe once or twice during a run of the application so willing to put up with this for now.
		// Also if the dimension (N) starts creeping up then this gets even slower but it will for sure work, which
		// was not guaranteed with the other approaches I took.

		i := 0;
		num_processed_basis_one_elements: int = 0;
		while true {
			if element & (1 << i) {
				if num_processed_basis_one_elements == grade-1 {
					break;	
				}	
				memcpy( cursor, one_element_symbols[i].data, one_element_symbols[i].count );
				cursor += one_element_symbols[i].count;
				memcpy( cursor, wedge.data, wedge.count );
				cursor += wedge.count;
				num_processed_basis_one_elements += 1;
			}
			
			i += 1;
		}
		memcpy( cursor, one_element_symbols[i].data, one_element_symbols[i].count );

		symbol.count = (grade * one_element_symbols[i].count) + (grade-1);
	}

	return symbol;
}
