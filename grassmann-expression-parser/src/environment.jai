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
	place.
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
		basis_element: *Basis_Element; // used in the multielement, points back to the basis element which contains the grade and symbol
	}
}

Multi_Element :: #type [] K_Element;

Symbol_Buffer :: struct {
	data:   *u8;
	size:   int;
	cursor: *u8;
}

Environment :: struct {
	
	MAX_DIMENSION :: 6;
	MAX_NUM_BASIS_ELEMENTS :: #run power( 2, MAX_DIMENSION );

	dimension: 			 int;
	n_element:           Basis_Element_Index;
	num_basis_elements:  int;

	basis_index_to_multielement_index: [MAX_NUM_BASIS_ELEMENTS] Multi_Element_Index;

	basis_symbols:  Symbol_Buffer;
	scalar_symbols: Symbol_Buffer;

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

	// set the one element symbol length; in the current implementation all one element symbols MUST be the same length
	// this is asserted if the user issues a command to change the current set of basis one elements
	{
		env.one_element_symbol_length = one_element_symbols[0].count;
	}

	// generate the basis elements from the space using the list of user-supplied one element symbols
	{
		// generate the basis elements of grade k and add them to our environment
		// symbols for the elements were initially added to the 'generate_basis_elements' but having the extra parameters
		// decreased readability, update symbols separately.

		if env.basis_elements.data != null {
			free( env.basis_elements.data );
			env.basis_elements.data = null;
			env.basis_elements.count = 0;
		}

		basis_elements: [..] Basis_Element;
		array_add( *basis_elements, Basis_Element.{ grade = 0, basis = 0 } );
		for j: 1..env.dimension {
			generate_basis_elements(
				 i 		 	    	 = 0
				,element 	    	 = Basis_Element.{ grade = 0, basis = 0 }
				,n       	    	 = env.dimension
				,k	     			 = j
				,basis_elements 	 = *basis_elements
			);
		}
		env.basis_elements = basis_elements;

		// generate the symbols for each of the basis elements (could do this as part of 'generate_basis_elements' but that procedure is fairly dense as is)
		
		// free the old basis element symbols buffer
		if env.basis_symbols.data != null {
			free( env.basis_symbols.data );
			env.basis_symbols.data   = null;
			env.basis_symbols.cursor = null;
			env.basis_symbols.size   = 0;
		}

		// allocate space to store all basis element symbols for the space of dimension N 
		for i: 1..env.dimension {	
			grade := i;
			num_exterior_product_symbols := grade-1;

			basis_elements_per_grade    := choose( env.dimension, grade );
			basis_element_symbol_length := (grade * env.one_element_symbol_length) + num_exterior_product_symbols; 

			env.basis_symbols.size += (basis_elements_per_grade * basis_element_symbol_length);
		}

		env.basis_symbols.data   = alloc( env.basis_symbols.size * size_of(u8) );
		env.basis_symbols.cursor = env.basis_symbols.data;
		assert( env.basis_symbols.data != null );
	
		for i: 0..env.num_basis_elements-1 {
			env.basis_elements[i].symbol = generate_basis_symbol( 
				 element = env.basis_elements[i].basis 
				,grade   = env.basis_elements[i].grade 
				,one_element_symbols
				,*env.basis_symbols
			);
		}
	}

	// update the environment's one-element symbols reference to point into the new one-element symbols in the basis symbol's buffer
	// the one elements are tightly packed so the lexer can make the determination whether an identifier is a basis element (1-element) or not
	{	
		if env.one_element_symbols.data != null {
			free( env.one_element_symbols.data );
			env.one_element_symbols.data  = null;
			env.one_element_symbols.count = 0; 
		}

		env.one_element_symbols.data  = alloc(  env.dimension * size_of(string) );
		env.one_element_symbols.count = env.dimension;
		assert( env.one_element_symbols.data != null );

		for i: 0..env.dimension-1 {
			env.one_element_symbols[i] = env.basis_elements[i+1].symbol; // +1 to skip over the grade 0 basis element
		}
	}

	// update the mapping table from an element's Basis_Element_Index to it's basis element in multielement order
	{
		for i: 0..env.num_basis_elements-1 {
			index := env.basis_elements[i].basis;
			env.basis_index_to_multielement_index[index] = i;
		}
	}

	return;	

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

choose :: (n: int, k: int) -> (value: int) {
	// example: choose(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * choose(n-1, k-1)) / k;
}

generate_basis_elements :: ( 
	 i: int
	,element: Basis_Element
	,n: int
	,k: int
	,basis_elements: *[..] Basis_Element
) {
	
	// This is a modified version of n choose k, where for each element in our one-elements we generate the combinations
	// with the element and the combinations without the element. The caller invokes this for k = 1..N so we end up
	// with each basis element in our collection in order of grade. Also, the one elements themselves are NOT supplied
	// as part of the call. Each one element is a bit field (1 << i) so when we bump i by one we are effectively dealing with
	// the next one element.

	if element.grade == k {
		// found a basis element of grade k, add to our collection of basis elements
		// the element.basis is filled in during execution of the procedure
		array_add( basis_elements, element );
		return;
	}

	if i >= n {
		// reached the end of the one elements without generating an element of grade k
		return;
	}

	// include the current one element (represented as 2^i so just a bit-shift) and find the basis-elements with this one-element and do the same excluding this current one-element
	include_first := Basis_Element.{ grade = element.grade + 1, basis = element.basis | (1 << i) }; 
	exclude_first := Basis_Element.{ grade = element.grade    , basis = element.basis };

	generate_basis_elements( i+1, include_first, n, k, basis_elements );
	generate_basis_elements( i+1, exclude_first, n, k, basis_elements );
}

// shouldn't need pop count
pop_count :: ( x: int ) -> ( bit_count: int ) {

	// Implementation from here: https://en.wikichip.org/wiki/population_count
	bit_count: int = 0;
	while x != 0 {
		bit_count += 1;
		x = x & (x-1);
	}
	
	return bit_count;
}

generate_basis_symbol :: ( element: int, grade: int, one_element_symbols: [] string, basis_symbols: *Symbol_Buffer ) -> ( symbol: string ) {

	symbol: string;
	
	wedge := "^";
	symbol.data = basis_symbols.cursor;
	if grade == 0 {
		// this might not be the correct play when printing one-elements
		s := "1";
		memcpy( basis_symbols.cursor, s.data, s.count );
		basis_symbols.cursor += s.count;
		symbol.count = s.count;
	}
	else {
		// Iterate through each bit and lookup the corresponding one element symbol and concat that into our symbol buffer.
		// Each basis element's .symbol field will point to a sub-section of the flat array of memory used by the basis symbols buffer.

		i := 0;
		num_processed_basis_one_elements: int = 0;
		while true {
			if element & (1 << i) {
				if num_processed_basis_one_elements == grade-1 {
					break;	
				}	

				memcpy( basis_symbols.cursor, one_element_symbols[i].data, one_element_symbols[i].count );
				basis_symbols.cursor += one_element_symbols[i].count;

				memcpy( basis_symbols.cursor, wedge.data, wedge.count );
				basis_symbols.cursor += wedge.count;

				num_processed_basis_one_elements += 1;
			}
			
			i += 1;
		}
		memcpy( basis_symbols.cursor, one_element_symbols[i].data, one_element_symbols[i].count );
		basis_symbols.cursor += one_element_symbols[i].count;

		symbol.count = (grade * one_element_symbols[i].count) + (grade-1);
	}

	return symbol;
}
