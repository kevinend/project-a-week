#scope_export

// have two types of symbols: 
// (1) basis symbols
// (2) scalar symbols
//
// we should build the symbols into this symbol section

// really you have basis_symbols
// scalar_symbols

// so now we have this notion of a 'Basis_Element' which is the display order, grade and symbol
// however, we might want to split this out

// basis_symbols
// scalar_symbol_buffer
// basis_elements, you need the same index into the array but i was thinking about putting them in a different display order
// 
// for the command you want to set the basis elements
// right now you have a single buffer to static memory for the basis_one_elements
// you need to have a buffer that can support up to 6 independent elements
// 
// right now you store all the symbols for each of the basis elements
//  e1, e2, e1e2, e3, e4, e1e3 .... whatever
// ideally you would like to point into here, but since bit-order is different from the grade order
//
// what you want is something like this
// e1 e2 e3 e4 <-- store a pointer into this array, but these are stored in display order
//
// [e1,e2,e1e2,e3...]
// [1 , 2 ,3  , 4...]
//
// env- basis_element_to_
// map one into the other
// can store the symbol and the grade together, which is what the basis element actually is
// basis_element_bit_index_to_basis_element

// basis_index_to_basis_element --> give it the raw index and then this returns a pointer to the element instead of the index

// you know the sizes so you could just pick from there as well...
// 2, 3, 4, 5, 6
// then you could free and reallocate that size

/*

Basis_Element :: struct {
	grade:  int;
	symbol: string; 
}

basis_elements: [MAX_NUM_BASIS_ELEMENTS] Basis_Element;

basis_index_to_basis_element: [MAX_NUM_BASIS_ELEMENTS] *Basis_Element; // this is the 'display index'
basis_element := basis_index_to_basis_element[element.basis]
basis_element.symbol =
basis_element.grade  =

then if you want the basis elements you grade basis_elements[1..env.dimension] up to the dimension and the count then you have your symbols
you are losing a bit by loading the grade every time, but there are at most 6 of these things

BASIS_\SYMBOL_MAX_SIZE :: 2; e1,e2,e3,e4,e5,e6
SCALAR_SYMBOL_MAX_SIZE :: 3; a0..a31 could support up to 64 characters

// or you just point into the buffer of strings directly...hmmm
// what i want is a packed set of strings since they are all the same length 
// if you know they are a fixed size then you only need a pointer 
// grade_one_basis_symbols: 	 *u8; //points into there and the size is the max size 
// grade_one_basis_symbols_size:  int; // then you just iterate through here and do the max size to jump sections and compare the sized

*/

// 2 bytes per one element
// number_of_grade_one_elements   * 2            (3*1)-1 
// number_of_grade_two_elements   * (5) (wedge)  (3*2)-1
// number_of_grade_three_elements * (8)          (3*3)-1
// number_of_grade_four_elements  * (11)         (3*4)-1
// number_of_grade_five_elements  * (14)         (3*5)-1
// number_of_grade_six_elements   * (17)		 (3*6)-1  // need the sum of all of these, to accomodate everything
// allocate the amount of space you need based on the grade
// if dimension == {
//   case 2; { basis_symbol_buffer = allocate( number_of_grade_one_elements * 2) }
//   case 3; { basis_symbol_buffer = allocate( ) }
//   case 4; { basis_symbol_buffer = allocate( ) }
//   case 5; { basis_symbol_buffer = allocate( ) }
//   case 6; { basis_symbol_buffer = allocate( ) }

// then you allocate this big ass block and store the strings in there
// as you iterate though how do you know where to store them? since they are all different sizes?
// you iterate through and then you store the values
// grade * grade_one_basis_symbol_size (3*2)-1
//
//
// basically every basis element needs to point into here and have the count associated with it
// [1, e1,e2,e3,e4,e1^e2,e1^e1]
// see how the strings are different sizes? that's okay as long as you have a string
// then for the one elements we point at the start and use the size of the one elements to handle what we want out of here
// i need to draw this up but i think it will actually work and not be terrible
//
// it is allocating the appropriate size
// could flip and do the case statement backwards and allocate all that space for the strings
// if dimension == 
//	case 6; { add to total; #through
//  case 5; { add to total; #through
//  case 4; { add to total; #through
//  case 3;
//  case 2; { 
//    add the number  
//    allocate the memory


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
	
	MAX_DIMENSION :: 6;
	MAX_NUM_BASIS_ELEMENTS :: #run power( 2, MAX_DIMENSION );

	ONE_ELEMENT_SYMBOL_NAME_MAX_SIZE :: 2;
	SCALAR_SYMBOL_NAME_MAX_SIZE      :: 2;

	dimension: int;
	one_element_symbols: [] string; 
	basis_elements:      [MAX_NUM_BASIS_ELEMENTS] Basis_Element;
	num_basis_elements:  int;
	n_element:           s64;

	one_element_symbol_buffer:  [MAX_DIMENSION * ONE_ELEMENT_SYMBOL_NAME_MAX_SIZE] u8;
	scalar_symbols_buffer:      [MAX_DIMENSION * SCALAR_SYMBOL_NAME_MAX_SIZE] u8;
	
}

environment_set_dimension_and_n_element :: ( env: *Environment, dimension: int ) {
	
	assert( dimension <= Environment.MAX_DIMENSION );
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
	assert( env.dimension <= Environment.MAX_DIMENSION );

	accum_elements_per_grade: [Environment.MAX_DIMENSION] int; // accumulated basis elements; number of elements at the grade specified + the num elements in any prior grade
	cursors: 				  [Environment.MAX_DIMENSION] int;

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
