/*
	This file implements a simple parser of Grassmann (exterior) algebra expressions.
	The structure of expressions handled by the parser are expressed as Backus-Naur Forms
	(BNF).

	As mentioned in the readme, Grassmann's products, especially the interior product of
	different graded elements is recursive in nature and cumbersome to expand by hand.
	This parser is written to allow an expansion phase to see the products fully expanded
	from an expression tree and then evaluated to return the final result.

	BNF for Grassmann expressions:
	 *The character R represents the 'Regressive' product. Don't want to get into unicode characters quite yet.
	  Eventually we will render those as logical and (U+2227) and or (U+2228) in unicode.

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"] ( <basis-one-element> | <number> |  | "(" <expression> ")" )

	NOTE:
	  Multiplication of basis elements by scalars is a tad clumsy.
	  You must specify multiplication using the exterior product. 3^e1 and not 3e1.
	  This isn't obvious until you try to parse the number 3 from 3e1 and use something like string-to-float
	  which e represents the exponent so it doesn't get read as a number with the additional e1 after it. It
	  gets read as 3e1 with the e interpreted as an exponent.

	  If your basis is not e1,e2,e3..eN then you could handle this differently but I don't want to restrict that
	  at this time so we are left with explicit exterior products.
*/

/*
	The code, as written, cannot handle k-vectors that are sums of basis elements where
	the grade of the k-vector > 1. 

	So a bivector in G(R3) == e1e2 + e1e3 can not be expressed as a single k-vector, it is
	getting added to a list as [e1e2|e1e3] though we may technically need to handle it as a single
	element (in the case of a span or cospan operation). Therefore, changing up the approach a bit here
	and want to introduce the idea of using the higher bits of the 'basis_elements' bit-vector to represent elements of grade > 1.

	Right now we use the lower bits; bits 0,1, and 2 to represent say e1, e2 and e3 in G(R3) and say that a bivector like e1e2 has
	both the e1 and e2 bits set. As mentioned above, this is not a technically feasible approach going forward so we want to use
	more bits from the 'basis_elements' bit-vector to represent higher-graded objects directly and NOT as a mish-mash of bits of the
	1-vectors.

	There is a scheme on the k-vectors that actually works out nicely but requires the use of offsets based on the grade.

	Easiest to show as an example. Assume G(R3) though this should work up to dimension 6 which has 64 basis-elements and we would run out of bits with a 64-bit integer.
	bits:      [0,   1,   2,   3,   4,    5,    6, ...]
	values:    [1,   2,   4,   8,   16,  32,   64, ...] // powers of two

	elements:  [e1, e2,  e3,  e12, e13, e23, e123, ...]
			   <---------->   <------------> <--->
			   	 1-vectors      2-vectors     3-vectors
			   ^Grade1-offset ^Grade2-offset ^Grade3-offset ...

	Setting the 'bits' indicates which basis elements are included in the k-vector. bit-0 means e1 is present, bit-1 means e2 is present, etc..
	The 'values' represent the integer value of that bit being set. These values are only important for the basis-1 elements in this scheme.

	Conceptually we divide the 'elements' array into sections based on the grade of the elements.
	So in the case of G(R3) we have a 3-element section for the 1-elements, followed by a 3-element section for the 2-elements followed by a 1-element section for the 3-elements.

	Those offsets are calculated a single time once we declare the dimension of the space and then we need to determine the bit-index for the element we are interested in.
	e1 is at bit-index 0 relative to it's starting offset, e1e2 is at bit-index 0 relative to it's starting offset and e1e2e3 is at bit-index 0 relative to it's starting offset.

	How do you calculate the bit-index for an element? The formula is the following.
		Bit-Index = (Value(Set-1-Bits)/Grade)-1


	Look at e1e2 which is the 4th bit (3rd index) in our bit-vector.
	If you set the bits, e1 and e2 the value of that integer is 3, if you divide this by the grade of the resulting element, 2 in this case, and subtract 1, you get the following:
	 	Bit-Index = (Sum/Grade)-1 = 3/2 - 1 = 0
    So if you offset into the 64-bit integer into the 2-vector 'section' you can set the 4th (index 3) bit and know that represents the value e1e2.

    After this example you assume that you can just sum the 1-vectors values and get the correct index but that assumption is incorrect.

    Look at e1e3 which is the 5th bit (4th index) in our bit-vector.
    Value(e1) + Value(e3) = 5, which is the correct bit but the incorrect index (should be 4).
    Applying the formula from above gives you:
    	Bit-Index = (Sum/Grade)-1 = (5/2)-1 = 1
    Which is the correct index that needs set once you offset into the Grade-2 basis elements section.
*/

/*
	TODO:
	--Complement, need LC and RC that proceed terms, needs added to the distribution since LC(e1+e2) == LC(e1) + LC(e2)
	--Tokenizer for the front-end (required to do the symbol lookup)
	--Symbol table so you can stash values, say you have a point P = e0 + e1 + e2, want to store that for reuse (then you have an interpreter effectively)
	--DeMorgan's Law, I ALWAYS forget this and it's impossible to remember
	--I'd like to be able to run simplify at the root of the tree (be left just with sums [convert differences to sums)
	--Return results as a string and have the tests pass or fail based on the output, a simple string compare
	--Less important but a stepwise expand, allow the user to click through and generate a string of the current state to see
	  how the process is working
	--Convert 'k-vector' into 'k-element' since we could have point spaces
*/

#scope_export

// globals:
dimension: int = 0;
basis_one_elements: [..] string;
n_element: int;
test_case: int = 0;

// data structures:
Node_Kind :: enum {
	K_Vector;
	Complement;  	 // left and right complements
	Binary_Operator; // exterior, regressive, interior, sum, difference
}

Expression_Node :: struct {
	kind: Node_Kind;
}

K_Vector_Node :: struct {

	// scalar, vector, bivector, trivector, ... , n-vector
	using _instance: Expression_Node;

	coefficient: float;
	grade: int;
	basis_components: s64; // bit field, we won't hit dimension 64 so I think we are okay here for now
}

Complement_Variant :: enum {
	 LEFT;
	 RIGHT;
}

Complement_Node :: struct {

	// left and right complement
	using _instance: Expression_Node;

	variant:  Complement_Variant;
	operand: *Expression_Node;
}

Binary_Operator_Node :: struct {

	// exterior, regressive, and interior products, +, -, *, / (division just handled for scalars now even though grassman division is supported)
	using _instance: Expression_Node;

	operation: u8;
	left_\operand: *Expression_Node;
	right_operand: *Expression_Node;
}

// procedures:
init_dimension :: ( N: int ) {

	// implmentation uses a single 64-bit integer to represent basis-one-elements.
	// dimension must be a positive integer and less than 64

	assert( N > 0  );
	assert( N < 64 ); 

	dimension = N;

	for i: 0..N-1 {
		n_element |= (1 << i);
	}

	return;
}

init_basis_one_elements :: ( one_element_symbols: [] string ) {

	// TODO: check that there aren't duplicates passed in here by the caller

	assert( one_element_symbols.count == dimension );

	if basis_one_elements.count > 0 { array_reset( *basis_one_elements ); }

	// copy the symbols over into our global symbol/basis elements table
	for one_element_symbol: one_element_symbols {
		array_add(*basis_one_elements, one_element_symbol );
	}

	return;
}

skip_spaces :: ( s: string ) -> ( cursor: string ) {
	cursor := s;
	while cursor && (cursor[0] == #char " " || cursor[0] == 9) { // horizontal tab
		advance( *cursor, 1 );
	}
	return cursor;
}

is_basis_one_element :: ( expression: string ) -> ( found_basis_element: bool, basis_element_index: s64 ) {

	string_compare :: ( s1: string, s2: string, n: int ) -> ( result: int, success: bool ) {

		// Invalid return values:
		// if the number of characters specified for comparison is larger than the number of characters in each string then return negative one and false

		// Valid return values:
		// zero means the strings are equal up to n bytes
		// negative one and a result of success means the strings are unequal and the character in the first string has a lesser value than the character in the second
		// positive one and a result of success means the strings are unequal and the character in the second string has a lesser value than the character in the first

		if s1.count < n || s2.count < n {
			return -1, false;
		}

		i: int = 0;
		while i < n && s1[i] == s2[i] {
			i += 1;
		}

		for i: 0..n-1 {
			if s1[i] == s2[i] { continue; }
			else
			if s1[i] < s2[i] { return -1, true; }
			else
			if s2[i] < s1[i] { return 1, true; }
		}

		return 0, true;
	}

	found_basis_element: bool = false;
	basis_element_index: s64  = -1;

	for i: 0..basis_one_elements.count-1 {
		if string_compare( expression, basis_one_elements[i], basis_one_elements[i].count ) == 0 {
			found_basis_element = true;
			basis_element_index = i;
			
			break;
		}
	}
	
	return found_basis_element, basis_element_index;
}

get_basis_one_element :: ( expression: string, basis_element_index: s64, is_negative: bool ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := expression;

	basis_one_element := basis_one_elements[basis_element_index];

	node := New(K_Vector_Node);
	node.kind   	 = Node_Kind.K_Vector;
	node.grade  	 = 1; 
	node.basis_components |= (1 << basis_element_index);

	if is_negative { node.coefficient = -1; }
	else           { node.coefficient =  1; }
	
	advance( *cursor, basis_one_element.count );

	return cast(*Expression_Node)node, cursor;
}

parse_expression :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	term: *Expression_Node = null;

	term, cursor = parse_exterior_term( cursor );
	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "+" || cursor[0] == #char "-") {
		operation: u8;
		next_term: *Expression_Node;

		operation = cursor[0];
		advance( *cursor, 1 );

		next_term, cursor = parse_exterior_term( cursor );

		node	  		   := New(Binary_Operator_Node);
		node.kind  		   = Node_Kind.Binary_Operator;
		node.left_operand  = term;
		node.operation     = operation;
		node.right_operand = next_term;

		term = cast(*Expression_Node)node;
	}

	return term, cursor;
}

parse_exterior_term :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	factor: *Expression_Node = null;

	factor, cursor = parse_exterior_factor( cursor );
	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "^" || cursor[0] == #char "R") {
		operation: u8;
		next_factor: *Expression_Node;

		operation = cursor[0];
		advance( *cursor, 1 );

		next_factor, cursor = parse_exterior_factor( cursor );

		node	  		   := New(Binary_Operator_Node);
		node.kind  		   = Node_Kind.Binary_Operator;
		node.left_operand  = factor;
		node.operation     = operation;
		node.right_operand = next_factor;

		factor = cast(*Expression_Node)node;
	}

	return factor, cursor;
}

parse_exterior_factor :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	node: *Expression_Node = null;

	factor_is_negative: bool = false;
	if cursor[0] == #char "-" {
		factor_is_negative = true;
		advance( *cursor, 1 );
	}

	is_basis_one_element, basis_element_index := is_basis_one_element( cursor );

	if is_basis_one_element {
		node, cursor = get_basis_one_element( cursor, basis_element_index, factor_is_negative );
	}
	else
	if is_digit( cursor[0] ) {
		success: bool;
		value: float;
		value, success, cursor = string_to_float( cursor );

		scalar_node := New(K_Vector_Node);
		scalar_node.kind   	    = Node_Kind.K_Vector;
		scalar_node.grade  	    = 0;

		if factor_is_negative { scalar_node.coefficient = -value; }
		else                  { scalar_node.coefficient =  value; }
		
		node = cast(*Expression_Node)scalar_node;
	}
	else
	if cursor[0] == #char "(" {
		advance( *cursor, 1 );
		node, cursor = parse_expression( cursor );

		cursor = skip_spaces( cursor );
		if cursor[0] == #char ")" {
			advance( *cursor, 1 );
		}
		else {
			print( "Error, unable to parse the expression, expected a terminating right parenthesis\n" );
			exit( -1 );
		}
	}
	else {
		print( "Error unable to parse the expression expected either a basis one element, number or subexpression wrapped in parens\n" );
		exit( -2 );
	}

	return node, cursor;
}

k_vector_to_string :: ( k_vector: *K_Vector_Node ) -> ( s: string ) {
	
	builder: String_Builder;
	num_processed_basis_elements: u8 = 0;

	// need a float to string to put the coefficient out front or a call to libc sprintf
	// punted on this and just used print statement for the coefficient then printed the string returned by this procedure.
	for i: 0..basis_one_elements.count-1 {
		if k_vector.basis_components & (1 << i) {
			append( *builder, basis_one_elements[i] );
			if num_processed_basis_elements < k_vector.grade-1 {
				// in expression like e1^e2 the number of exterior products is the grade minus 1
				append( *builder, "^" );
			}
			num_processed_basis_elements += 1;
		}
	}

	// MEMORY LEAK!!!
	// no idea how to free the builder in this language!
	s: string = builder_to_string( *builder );

	return s;
}

print_expression_tree :: ( node: *Expression_Node, indent: int = 0 ) {

	for i: 0..indent-1 {
		print( " " );
	}

	if node.kind == Node_Kind.K_Vector {
		
		k_vector := cast(*K_Vector_Node)node;
 
		if k_vector.grade > 0 {
			k_vector_as_string := k_vector_to_string( k_vector );
			print("%1%2\n", k_vector.coefficient, k_vector_as_string );
		}
		else {
			print( "%1\n", k_vector.coefficient );
		}
	}
	else 
	if node.kind == Node_Kind.Binary_Operator {
		binary_operator := cast(*Binary_Operator_Node)node;
		
		if binary_operator.operation == {
			case #char "^"; { print( "^\n" ); }
			case #char "R"; { print( "R\n" ); }
			case #char "+"; { print( "+\n" ); }
			case #char "-"; { print( "-\n" ); }
			case; print( "Unhandled operator case in print procedure.\n" );
		}

		print_expression_tree( binary_operator.left\_operand, indent + 2 );
		print_expression_tree( binary_operator.right_operand, indent + 2 );
	}

	return;
}

print_expression_from_expression_tree :: ( node: *Expression_Node ) {

	if node.kind == Node_Kind.K_Vector {	
		k_vector := cast(*K_Vector_Node)node;
 
		if k_vector.grade > 0 {
			k_vector_as_string := k_vector_to_string( k_vector );
			print( "%", k_vector_as_string );
		}
		else {
			print( "%", k_vector.coefficient );
		}
	}
	else 
	if node.kind == Node_Kind.Binary_Operator {
		binary_operator := cast(*Binary_Operator_Node)node;

		print_expression_from_expression_tree( binary_operator.left\_operand );

		if binary_operator.operation == {
			case #char "^"; { print( "^" ); }
			case #char "+"; { print( " + " ); }
			case #char "-"; { print( " - " ); }
			case; print( "Unhandled operator case in print procedure.\n" );
		}

		print_expression_from_expression_tree( binary_operator.right_operand );
	}

	return;
}

clone_k_vector_node :: ( k_vector_node: *K_Vector_Node ) -> ( cloned_node: *K_Vector_Node ) {

	cloned_node := New(K_Vector_Node);
	cloned_node.kind  			 = Node_Kind.K_Vector;
	cloned_node.grade 			 = k_vector_node.grade;
	cloned_node.coefficient 	 = k_vector_node.coefficient;
	cloned_node.basis_components = k_vector_node.basis_components;

	return cloned_node;
}

clone_complement_node :: ( complement_node: *Complement_Node ) -> ( cloned_node: *Complement_Node ) {

	cloned_node 		 := New(Complement_Node);
	cloned_node.kind 	  = Node_Kind.Complement;
	cloned_node.variant   = complement_node.variant;
	cloned_node.operand   = clone_node( complement_node.operand );

	return cloned_node;
}

clone_binary_operator_node :: ( binary_operator_node: *Binary_Operator_Node ) -> ( cloned_node: *Binary_Operator_Node ) {

	cloned_node := New(Binary_Operator_Node);
	cloned_node.kind 	  	= Node_Kind.Binary_Operator;
	cloned_node.operation 	= binary_operator_node.operation;
	cloned_node.left\_operand = clone_node( binary_operator_node.left\_operand );
	cloned_node.right_operand = clone_node( binary_operator_node.right_operand );

	return cloned_node;
}

clone_node :: ( node: *Expression_Node ) -> ( cloned_node: *Expression_Node ) {
		
	if node.kind == Node_Kind.K_Vector {
		return cast(*Expression_Node)clone_k_vector_node( cast(*K_Vector_Node)node );
	}
	else
	if node.kind == Node_Kind.Complement {
		return cast(*Expression_Node)clone_complement_node( cast(*Complement_Node)node );
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		return cast(*Expression_Node)clone_binary_operator_node( cast(*Binary_Operator_Node)node );
	}
	else {
		print( "Unrecognized node kind found when cloning nodes\n" );
		return null;
	}
}

is_grassmann_operator :: ( op: u8 ) -> (bool) {
	if op == #char "^" || op == #char "R" {
		return true;
	}
	else {
		return false;
	}
}

distribute_grassmann_products :: ( node: *Expression_Node ) -> ( tree: *Expression_Node ) {

	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.
	 
	subtree: *Expression_Node;
	if node.kind != Node_Kind.Binary_Operator {
		subtree = clone_node( node );	
	}
	else {
		// Determine whether we need to distribute the grassmann products over any sums or differences.
		// The boolean values are introduced to reduce the level of nesting that happens if you don't 
		// do it that way (it was very hard to visually parse and a lot of duplicate 'clone the current node because
		// we don't need to distribute')

		binary_operator_node := cast(*Binary_Operator_Node)node;
		left\_operator: *Binary_Operator_Node = null;
		right_operator: *Binary_Operator_Node = null;

		distribute_over_left\_operand := false;
		distribute_over_right_operand := false;

		if is_grassmann_operator(binary_operator_node.operation) {
			if binary_operator_node.left_operand.kind == Node_Kind.Binary_Operator {
				left_operator = cast(*Binary_Operator_Node)binary_operator_node.left_operand;
				if left_operator.operation == #char "+" || left_operator.operation == #char "-" {
					distribute_over_left_operand = true;
				}
			}
			else if binary_operator_node.right_operand.kind == Node_Kind.Binary_Operator {
				right_operator = cast(*Binary_Operator_Node)binary_operator_node.right_operand;
				if right_operator.operation == #char "+" || right_operator.operation == #char "-" {
					distribute_over_right_operand = true;
				}	
			}
		}

		if distribute_over_left_operand {
			// distribute the argument on the right over the argument on the right
			distributive_operator    := binary_operator_node.operation;
			distribute_over_operator := left_operator.operation;

			new_binary_operator_node := New(Binary_Operator_Node);

			new_\left_node := New(Binary_Operator_Node);
			new_right_node := New(Binary_Operator_Node);

			new_left_node.kind           = Node_Kind.Binary_Operator;
			new_left_node.operation 	 = distributive_operator;
			new_left_node.left_operand   = clone_node( cast(*Expression_Node)left_operator.left_operand );
			new_left_node.right_operand  = clone_node( cast(*Expression_Node)binary_operator_node.right_operand );

			new_right_node.kind          = Node_Kind.Binary_Operator;
			new_right_node.operation 	 = distributive_operator;
			new_right_node.left_operand  = clone_node( cast(*Expression_Node) left_operator.right_operand ); 
			new_right_node.right_operand = clone_node( cast(*Expression_Node) binary_operator_node.right_operand );

			new_binary_operator_node.kind          = Node_Kind.Binary_Operator;
			new_binary_operator_node.operation     = distribute_over_operator; // + or -
			new_binary_operator_node.left\_operand = distribute_grassmann_products( cast(*Expression_Node)new_left\_node );
			new_binary_operator_node.right_operand = distribute_grassmann_products( cast(*Expression_Node)new_right_node );

			subtree = cast(*Expression_Node)new_binary_operator_node;
		}
		else
		if distribute_over_right_operand {
			// distribute the argument on the left over the argument on the right
			distributive_operator    := binary_operator_node.operation;
			distribute_over_operator := right_operator.operation;

			new_binary_operator_node := New(Binary_Operator_Node);

			new_\left_node := New(Binary_Operator_Node);
			new_right_node := New(Binary_Operator_Node);

			new_left_node.kind           = Node_Kind.Binary_Operator;
			new_left_node.operation 	 = distributive_operator;
			new_left_node.left_operand   = clone_node( cast(*Expression_Node)binary_operator_node.left_operand );
			new_left_node.right_operand  = clone_node( cast(*Expression_Node)right_operator.left_operand );

			new_right_node.kind          = Node_Kind.Binary_Operator;
			new_right_node.operation 	 = distributive_operator;
			new_right_node.left_operand  = clone_node( cast(*Expression_Node)binary_operator_node.left_operand );
			new_right_node.right_operand = clone_node( cast(*Expression_Node)right_operator.right_operand );
			
			new_binary_operator_node.kind          = Node_Kind.Binary_Operator;
			new_binary_operator_node.operation     = distribute_over_operator; // + or -
			new_binary_operator_node.left\_operand = distribute_grassmann_products( cast(*Expression_Node)new_left\_node );
			new_binary_operator_node.right_operand = distribute_grassmann_products( cast(*Expression_Node)new_right_node );

			subtree = cast(*Expression_Node)new_binary_operator_node;
		}
		else {
			subtree = clone_node( node );
		}
	}

	return subtree;
}

generate_k_span   :: ( element: *K_Vector_Node, k: int ) -> ( k_span: [..] *K_Vector_Node ) {
	
	// to generate a k-span you need the 1-span
	one_span: [..] int;
	for bit: 0..dimension-1 {
		if element.basis_components & (1 << bit) {
			array_add( *one_span, bit );
		}
	}

	k_span: [..] *K_Vector_Node;
	k_element     := New(K_Vector_Node);
	k_element.kind = Node_Kind.K_Vector;
	if k == 1 {
		for i: 0..one_span.count-1 {
			k_element.grade 	  = 1;
			k_element.coefficient = 1;
			k_element.basis_components |= (1 << one_span[i]);

			array_add( *k_span, clone_k_vector_node( k_element ) );

			k_element.basis_components &= 0; // reset/clear bits
		}
	}
	else {
		// build the set of k-elements (k-combinations) from the one-span.
		// this is O(N^3), probably a better way to do this!
		one_element := New(K_Vector_Node);
		for i: 0..one_span.count-1 {

			one_element.grade = 1;
			one_element.coefficient = 1;
			one_element.basis_components &= 0;
			one_element.basis_components |= (1 << one_span[i]);
			
			k_element := clone_k_vector_node( one_element );

			for j: i+1..one_span.count-1 {

				num_remaining_one_elements := one_span.count-j;
		
				if num_remaining_one_elements >= (k-1) {	
					s: int = 0;
					while k_element.grade != k {
						k_element.basis_components |= (1 << one_span[j+s]);
						k_element.grade += 1;
						s += 1;
					}

					array_add( *k_span, k_element );

					k_element = clone_k_vector_node( one_element );
				}
			}
		}
	}

	return k_span;
}

anticommutative_coefficient :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( coefficient: int ) {

	// This procedure calculates the number of swaps required to reorder the elements into the 'canonical' ordering.
	// If the number of swaps is odd then the product needs negated.

	// The current implementation forces an ordering of 1-elements such that the exterior product of (e2^e1)^e3
	// will automatically be reordered to the positive (scalar factor is positive) product e1^e2^e3.

	// Algorithm:
	// Take the exterior product between the elements and handle the anticommutativity property of the grassmann product.
	// For every bit (basis one element) in the LEFT argument
	//	Start at right-most bit in the LEFT arg
	//  Find it's place in the RIGHT arg and set that bit.
	//	Anytime you cross over a bit in the RIGHT that is set, then you have swapped the elements.
	// Rinse and repeat for every other bit in the LEFT.
	//
	// If after completing this process we have swapped an odd number of times then we negate the final product.

	temp: int = rvalue.basis_components;
	num_swaps: int = 0;
	for < i: dimension-1..0 {
		if lvalue.basis_components & (1 << i) {
			for j: 0..dimension-1 {
				if i == j { 
					temp |= (1 << j);
					break;
				}
				else 
				if temp & (1 << j) { 
					num_swaps += 1;
				}
			}
		}
	}

	if num_swaps % 2 == 0 { return 1; }
	else { return -1; }
}

generate_k_cospan :: ( m_element: *K_Vector_Node, k_span: [..] *K_Vector_Node ) -> ( k_cospan: [..] *K_Vector_Node ) {

	co_m_element :: ( m_element: *K_Vector_Node, k_element: *K_Vector_Node ) -> ( m_k_element: *K_Vector_Node ) {

		// similar to the 'complement' however this is with respect to an m-element instead of the basis n-element.
		
		m_k_element := New(K_Vector_Node);
		m_k_element.kind  		= Node_Kind.K_Vector;
		m_k_element.grade 		= m_element.grade - k_element.grade;
		m_k_element.coefficient = 1;
		m_k_element.basis_components = m_element.basis_components ^ k_element.basis_components;

		m_k_element.coefficient *= anticommutative_coefficient( k_element, m_k_element );

		return m_k_element;
	}

	k_cospan: [..] *K_Vector_Node;
	for k_element: k_span {
		m_k_element := co_m_element( m_element, k_element );
		array_add( *k_cospan, m_k_element );
	}

	return k_cospan;
}

simplify :: ( node: *Expression_Node ) -> ( result: *Expression_Node ) {

	// This procedure can be called on any node in an expression tree.
	// The reason it can't always return a k-vector is that sums of k-vectors of different grades are not defined.

	result: *Expression_Node;
	if node.kind == Node_Kind.K_Vector {	
		result = clone_node( node );
	}
	else
	if node.kind == Node_Kind.Complement {
		complement_node := cast(*Complement_Node)node;

		// TODO: Need to distribute the complement over sums -- LC(e1+e2) == LC(e1) + LC(e2)
		// result = cast(*Expression_Node)complement( complement_node.variant, complement_node
		result = clone_node( node );
	}
	else {
		binary_operator_node := cast(*Binary_Operator_Node)node;
		binary_operator_node.left\_operand = simplify( binary_operator_node.left\_operand );
		binary_operator_node.right_operand = simplify( binary_operator_node.right_operand );

		if binary_operator_node.left\_operand.kind == Node_Kind.K_Vector && binary_operator_node.right_operand.kind == Node_Kind.K_Vector {
			left  := cast(*K_Vector_Node)binary_operator_node.left\_operand;
			right := cast(*K_Vector_Node)binary_operator_node.right_operand;

			operation := binary_operator_node.operation;
			if operation == #char "^" {
				result = cast(*Expression_Node)exterior_product( left, right );
			}
			else
			if operation == #char "R" {
				result = cast(*Expression_Node)regressive_product( left, right );
			}
			else 
			if operation == #char "+" || operation == #char "-" {
				sum := clone_k_vector_node( left );
				if left.grade == right.grade {
					if operation == #char "-" { right.coefficient *= -1; } // part of expand could be converting differences into sums
					sum.coefficient += right.coefficient;
					result = cast(*Expression_Node)sum;
				}
				else {
					// sum is of two k-vectors of different grade, can't sum them, just clone the node
					result = clone_node( node );
				}
			}
		}
		else {
			// one node is a sum and we can't simplify it further, just clone the node
			result = clone_node( node );
		}
	}
	
	return result;
} 

// TODO: Combine this and 'distribute_grassmann_products' into a single 'expand' function
common_factor_theorem :: ( node: *Expression_Node ) -> ( tree: *Expression_Node ) {

	subtree: *Expression_Node;
	if node.kind != Node_Kind.Binary_Operator {
		subtree = clone_node( node );
	}
	else {
		// Determine whether we need to apply the common factor theorem to any regressive products.
		binary_operator_node := cast(*Binary_Operator_Node)node;
		if binary_operator_node.operation == #char "R" {
			left  := cast(*K_Vector_Node)simplify( binary_operator_node.left\_operand );
			right := cast(*K_Vector_Node)simplify( binary_operator_node.right_operand );

			if (left.grade + right.grade) < dimension {
				// regressive product is zero, don't bother expanding
				subtree = clone_node( node );
			}
			else {
				// generate the k-span and k-cospan of the right node.
				// k represents the grade of the element the left and right share in common.
				// Example:
				//	 N = 4
				//   A = e1^e3
				//   B = e2^e3^e4
				//   k = (Grade(A) + Grade(B)) - N == (2+3) - 4 == 5-4 = 1
				// 1-span:	 [e2,e3,e4]
				// 1-cospan: [e3e4, -e2e4, e2e3]

				k := (left.grade + right.grade) - dimension;
				k_span:	  [..] *K_Vector_Node = generate_k_span\ ( right, k );
				k_cospan: [..] *K_Vector_Node = generate_k_cospan( right, k_span );

				// build the regressive product subtrees from the k-span and k-cospan elements.
				// Example: using the span and cospan from the above example
				// regressive_product_subtrees = [subtree0, subtree1, subtree2, ...]
				//   where subtree0 =		R
				//						^     k-span[0]
				//				 	  L  k-cospan[0]

				regressive_product_subtrees: [..] *Binary_Operator_Node;
				for i: 0..k_span.count-1 {

					regressive_product_node := New(Binary_Operator_Node);
					regressive_product_node.kind = Node_Kind.Binary_Operator;
					regressive_product_node.operation = #char "R";
				
					exterior_product_node := New(Binary_Operator_Node);
					exterior_product_node.kind = Node_Kind.Binary_Operator;
					exterior_product_node.operation = #char "^";
					exterior_product_node.left\_operand = cast(*Expression_Node)clone_k_vector_node( left ); 
					exterior_product_node.right_operand = cast(*Expression_Node)k_cospan[i];

					regressive_product_node.left\_operand = cast(*Expression_Node)exterior_product_node;
					regressive_product_node.right_operand = cast(*Expression_Node)k_span[i];

					array_add( *regressive_product_subtrees, regressive_product_node );
				}

				// generate the new root node with the sums of the regressive product subtrees created above
				// Example: convert the subtrees into a tree 'sum', provided there is more than one subtree
				// regressive_product_subtrees: [subtree0, subtree1, subtree2, subtree3]
				//      		+
				// 		subtree0    +
				//      	subtree1 	+
				//        		subtree2 subtree3


				if regressive_product_subtrees.count == 1 {
					subtree = cast(*Expression_Node)regressive_product_subtrees[0];
				}
				else {

					root := New(Binary_Operator_Node);
					root.kind = Node_Kind.Binary_Operator;
					root.operation = #char "+";
					
					i: int = 0;
					current := root;
					while i < regressive_product_subtrees.count-2 { 
						current.left_operand = cast(*Expression_Node)regressive_product_subtrees[i];
						
						next := New(Binary_Operator_Node);
						next.kind 	   = Node_Kind.Binary_Operator;
						next.operation = #char "+";

						current.right_operand = cast(*Expression_Node)next;
						current = next;

						i += 1;
					}

					// the final sum needs handled outside the loop
					current.left\_operand = cast(*Expression_Node)regressive_product_subtrees[i];
					current.right_operand = cast(*Expression_Node)regressive_product_subtrees[i+1];

					subtree = cast(*Expression_Node)root;
				}
			}
		}
		else {
			// binary operator node is not a regressive product, just clone the node and return it
			subtree = clone_node( node );
		}
	}

	return subtree;
}
	
flatten :: ( subtree: *Expression_Node ) -> ( operators: [..] u8, elements: [..] K_Vector_Node ) {
	
	// In-order traversal of the distributed expression tree.
	// Once we have the flattened list of operators (sums/differences/grassmann products) and elements (1-vector/basis 1 elements)
	// we need to evaluate the possible cases of our tree.
	//
	
	flatten_helper :: ( node: *Expression_Node, operators: *[..] u8, elements: *[..] K_Vector_Node ) {

		success: bool;
		
		if node.kind == Node_Kind.K_Vector {
			k_vector_node := cast(*K_Vector_Node)node;
			
			k_vector: K_Vector_Node;
			k_vector.kind 		 	  = Node_Kind.K_Vector;
			k_vector.coefficient 	  = k_vector_node.coefficient;
			k_vector.grade       	  = k_vector_node.grade;
			k_vector.basis_components = k_vector_node.basis_components;

			array_add( elements, k_vector );
		}
		else
		if node.kind == Node_Kind.Binary_Operator {
			binary_operator_node := cast(*Binary_Operator_Node)node;
			flatten_helper( binary_operator_node.left\_operand, operators, elements );

			array_add( operators, binary_operator_node.operation );

			flatten_helper( binary_operator_node.right_operand, operators, elements );
		}
	}

	operators: [..] u8;
	elements:  [..] K_Vector_Node;

	flatten_helper( subtree, *operators, *elements );

	return operators, elements;
}

exterior_product :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( result: *K_Vector_Node ) {

	// assume the result is zero, change it if it isn't the case
	result 		:= New(K_Vector_Node);
	result.kind  = Node_Kind.K_Vector;
	result.coefficient      = 0;
	result.grade 			= 0;
	result.basis_components = 0;

	if lvalue.grade == 0 || rvalue.grade == 0 {
		result.grade       	    = lvalue.grade + rvalue.grade;
		result.basis_components = lvalue.basis_components | rvalue.basis_components;
		result.coefficient 	    = lvalue.coefficient * rvalue.coefficient;	
	}
	else
	if (lvalue.basis_components & rvalue.basis_components) == 0 { // no one elements in common; implies sum of grades is less than or equal to N
		
		result.grade 			= lvalue.grade + rvalue.grade;
		result.coefficient  	= lvalue.coefficient * rvalue.coefficient;
		result.basis_components = lvalue.basis_components | rvalue.basis_components;

		result.coefficient *= anticommutative_coefficient( lvalue, rvalue );
	}

	return result;
}

regressive_product :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( result: *K_Vector_Node ) {

	// assume the result is zero, change it if it isn't the case
	result      := New(K_Vector_Node);
	result.kind  = Node_Kind.K_Vector;
	result.basis_components = 0;
	result.grade 			= 0;
	result.coefficient      = 0;

	if lvalue.grade == dimension {
		// return the right-operand 
		result.grade 	   		= rvalue.grade;
		result.coefficient 		= rvalue.coefficient * lvalue.coefficient;
		result.basis_components = rvalue.basis_components;
	}
	else 
	if rvalue.grade == dimension {
		// return the left-operand
		result.grade 			= lvalue.grade;
		result.coefficient 		= lvalue.coefficient * rvalue.coefficient;
		result.basis_components = rvalue.basis_components; 
	}

	return result;
}

// instead of flatten, we have eval that allows you to evaluate from certain points in the tree
// so flatten traverses the entire tree but what eval should do is generate a tree that is just sums
// then if we have sums or whatever, then you don't necessarily have to flatten the tree if you don't want to
// you could kind of just leave it and generate the expression if you want to...decisions, decisions
// i really like the idea of eval 
// if you can resolve the sum, go ahead, if you can't then you can't, 
// or difference, then you can flatten and remove sums if need be
// 
eval :: ( operators: [] u8, elements: [] K_Vector_Node ) -> ( k_vectors: [..] K_Vector_Node ) {

		bubble_sort :: ( k_vectors: *[] K_Vector_Node ) {

		swap: bool     = false;
		for _: 0..k_vectors.count-1 {
			for i: 0..k_vectors.count-2 {
				if 	(<<k_vectors)[i+1].grade <= (<<k_vectors)[i].grade && 
			   		(<<k_vectors)[i+1].basis_components < (<<k_vectors)[i].basis_components {

					temp     	  	          := (<<k_vectors)[i];
					(<<k_vectors)[i]   = (<<k_vectors)[i+1];
					(<<k_vectors)[i+1] = temp;

					swap = true;
				}
			}

			if !swap { return; }
			swap = false;
		}
	}

	// Possible cases with the operators and elements:
	//	1. No operators need applied, return the element
	//		operators: []
	//		elements : [k-vector]
	//
	// 2. Grassmann products only; apply the operators to build the resulting single k-vector element
	//      operators: [^,^,^..] 
	//		elements:  [k-vector1, k-vector2, k-vector3...]
	//
	// 3. Sums and difference only; sort the elements so k-vectors of same basis components can be summed
	//		operators: [+,-,+]
	//		elements:  [e1,e2,e1]
	//
	//		sort( elements );
	//
	//		operators: [+,+,+] 
	//		elements: [e1,e1,-e2]
	//
	// 4. Grassmann products and sums/differences; resolve grassmann products, sort then sum elements.
	//		operators: [^^+^]
	//		elements:  [e1,e2,e3,e2,e3]
	//
	//		Post grassmann eval:
	//		operators: [+]
	//		elements:  [e1e2e3,e2e3]
	//
	// 		sort( elements );
	//		
	//		operators: [+]
	//		elements: [e2e3,e1e2e3]
	//
	//		Final Result:
	//		e2^e3 + e1^e2^e3

  
  	// When this is a single element this process is wasteful.
  	// We 'dequeue' the element and 'enqueue' it twice before returning it as the result.
  	// However, it does provide a nice flat structure to the code.

  	success: bool;

 	lvalue: K_Vector_Node;
 	rvalue: K_Vector_Node;
 	op: u8;

 	products: [..] K_Vector_Node; // if single element think 1^e1 == e1 or N \/ e1 = e1

	lvalue = elements[0];
	for i: 0..operators.count-1 {
		op = operators[i];
		rvalue = elements[i+1];
		if op == #char "^" { 
			lvalue = exterior_product( *lvalue, *rvalue ); 
		}
		else
		if op == #char "R" {
			lvalue = regressive_product( *lvalue, *rvalue );
		}
		else
		if op == #char "+" || op == #char "-" {
			if op == #char "-" { rvalue.coefficient *= -1; }
			array_add( *products, lvalue );
			lvalue = rvalue;
		}
	}
	array_add( *products, lvalue );

	bubble_sort( *products );

	results: [..] K_Vector_Node;

	lvalue = products[0];
	for i: 1..products.count-1 {
		rvalue = products[i];
		if lvalue.basis_components == rvalue.basis_components {
			lvalue.coefficient += rvalue.coefficient;
		}
		else {
			array_add( *results, lvalue );
			lvalue = rvalue;
		}
	}
	array_add( *results, lvalue );

	return results;
}

complement :: ( variant: Complement_Variant, k_vector: *K_Vector_Node ) -> ( n_k_vector: *K_Vector_Node ) {
	
	n_k_vector := New(K_Vector_Node);
	n_k_vector.kind             =  Node_Kind.K_Vector;
	n_k_vector.grade 	    	=  dimension - k_vector.grade;
	n_k_vector.coefficient 	 	=  k_vector.coefficient;
	n_k_vector.basis_components =  k_vector.basis_components ^ n_element; // XOR

	// Need to determine whether the (n-k)-element should be negated.
	// The procedure below effectively takes the exterior product between the args and determines the number
	// of swaps required to build the final N element from the product of the (n-k)-element or the k-element
	// or vice versa depending on whether this is a left or right complement.

	if variant == Complement_Variant.LEFT {
		n_k_vector.coefficient *= anticommutative_coefficient( n_k_vector, k_vector );
	} 
	else
	if variant == Complement_Variant.RIGHT {
		n_k_vector.coefficient *= anticommutative_coefficient( k_vector, n_k_vector );
	}

	return n_k_vector;
}

test :: ( 
	 dimension: u8
	,basis: [] string
	,expression: string
	,should_print_expression_tree_post_parsing: 	 bool = true
	,should_print_expression_tree_post_distribution: bool = true
	,should_print_expression_tree_post_common_factor_theorem:   bool = true
	,should_print_expression_tree_post_grassmann_operator_eval: bool = true
) {

	test_case += 1;

	init_dimension( N = dimension );
	init_basis_one_elements( basis );

	print( "Test case %:\n", test_case );
	print( "Original Expression: %\n", expression );

	expression_tree: *Expression_Node = parse_expression( expression );
	if should_print_expression_tree_post_parsing {
		print( "Initial expression tree\n" );
		print_expression_tree( expression_tree );
		print( "\n" );
	}

	expression_tree_post_distribution := distribute_grassmann_products( expression_tree );
	if should_print_expression_tree_post_distribution {
		print( "Expression tree post distribution of grassmann products:\n" );
		print_expression_tree( expression_tree_post_distribution );
	}

	expression_tree_post_common_factor_theorem := common_factor_theorem( expression_tree_post_distribution );
	if should_print_expression_tree_post_common_factor_theorem {
		print( "Expression tree post application of the common factor theorem:\n" );
		print_expression_tree( expression_tree_post_common_factor_theorem );
	}

	operators, elements := flatten( expression_tree_post_common_factor_theorem );

	results := eval( operators, elements );

	// print_simplified_expression()
	for result: results {
		k_vector_as_string := k_vector_to_string( *result );
		print( "%1%2 ", result.coefficient, k_vector_as_string );
	}
	print( "\n" );

	return;
}

main :: () {

// THIS SECTION IS COMPLEMENT TESTING!
{
	init_dimension( 3 );
	init_basis_one_elements( string.["e1","e2","e3"] );

	e1: K_Vector_Node;
	e1.basis_components = 1;
	e1.grade = 1;
	e1.coefficient = 1;

	e2e3 := complement( .RIGHT, *e1 );
	print_expression_tree( cast(*Expression_Node)e2e3 );

}

{
	init_dimension( 3 );
	init_basis_one_elements( string.["e1","e2","e3"] );

	e2: K_Vector_Node;
	e2.basis_components = 2;
	e2.grade = 1;
	e2.coefficient = 1;

	e1e3 := complement( .RIGHT, *e2 );
	print_expression_tree( cast(*Expression_Node)e1e3 );
}
 
{

	init_dimension( 3 );
	init_basis_one_elements( string.["e1","e2","e3"] );

	e1e3: K_Vector_Node;
	e1e3.basis_components = 5;
	e1e3.grade = 2;
	e1e3.coefficient = 1;

	e2 := complement( .RIGHT, *e1e3 );
	print_expression_tree( cast(*Expression_Node)e2 );

}

{
	init_dimension( 2 );
	init_basis_one_elements( string.["e1","e2"] );

	e2: K_Vector_Node;
	e2.basis_components = 2;
	e2.grade = 1;
	e2.coefficient = 1;

	e1 := complement( .RIGHT, *e2 );
	print_expression_tree( cast(*Expression_Node)e1 );
}

{
	init_dimension( 2 );
	init_basis_one_elements( string.["e1","e2"] );

	_e1: K_Vector_Node;
	_e1.basis_components = 1;
	_e1.grade = 1;
	_e1.coefficient = 1;

	_e2 := complement( .RIGHT, *_e1 );
	print_expression_tree( cast(*Expression_Node)_e2 );
}

{
	init_dimension( 4 );
	init_basis_one_elements( string.["e1","e2","e3","e4"] );

	e12: K_Vector_Node;
	e12.basis_components = 3;
	e12.grade = 2;
	e12.coefficient = 1;

	e34 := complement( .RIGHT, *e12 );
	print_expression_tree( cast(*Expression_Node)e34 );
}

{

	init_dimension( 4 );
	init_basis_one_elements( string.["e1","e2","e3","e4"] );

	e1: K_Vector_Node;
	e1.basis_components = 1;
	e1.grade = 1;
	e1.coefficient = 1;

	e234 := complement( .RIGHT, *e1 );
	print_expression_tree( cast(*Expression_Node)e234 );
}

{

	init_dimension( 4 );
	init_basis_one_elements( string.["e1","e2","e3","e4"] );

	e2: K_Vector_Node;
	e2.basis_components = 2;
	e2.grade = 1;
	e2.coefficient = 1;

	e134 := complement( .RIGHT, *e2 );
	print_expression_tree( cast(*Expression_Node)e134 );
}


// THE FOLLOWING TWO SECTIONS ARE THE GENERATE-K-SPAN and GENERATE-K-COSPAN TESTS

/*
{
	k_vector := New(K_Vector_Node);
	k_vector.grade = 4;
	for i: 0..3 {
		k_vector.basis_components |= (1 << i);
	}
	print( "Basis components == %\n", k_vector.basis_components );

	m_k_cospan: [..] K_Vector_Node;
	m_k_element: K_Vector_Node;

	k_span := generate_k_span( k_vector, 1 );
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_span[i].grade, k_span[i].coefficient, k_span[i].basis_components );
	}
	print( "\n" );

	k_span = generate_k_span( k_vector, 2);
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_span[i].grade, k_span[i].coefficient, k_span[i].basis_components );
	}
	print( "\n" );
	

	k_span = generate_k_span( k_vector, 3); // this doesn't work in a 4-space,
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_span[i].grade, k_span[i].coefficient, k_span[i].basis_components );
	}
	print( "\n" );

	// 1 2 4 8 
	// 

	print( "This is for the 4 span of the element\n" );
	k_span = generate_k_span( k_vector, 4);
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_span[i].grade, k_span[i].coefficient, k_span[i].basis_components );
	}
	print( "\n" );
}

{
	m_element := New(K_Vector_Node);
	m_element.grade = 3;
	m_element.basis_components |= (1 << 0); // e1
	m_element.basis_components |= (1 << 2); // e3
	m_element.basis_components |= (1 << 3); // e4

	k_span   := generate_k_span( m_element, 1 );
	k_cospan := generate_k_cospan( m_element, k_span );

	print( "SPAN:\n" );
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_span[i].grade, k_span[i].coefficient, k_span[i].basis_components );
	}
	print( "\n" );

	// the number of swaps doesn't look right, but we need to take a mental break i can feel it, time for a walk and X
	print( "COSPAN:\n" );
	for i: 0..k_span.count-1 {
		print( "(Grade,Coefficient,Basis-Components) == (%,%,%)\n", k_cospan[i].grade, k_cospan[i].coefficient, k_cospan[i].basis_components );
	}
	print( "\n" );
}
*/

// THE FOLLOWING SECTIONS ARE FOR TESTING THE EXPANSION OF THE COMMON FACTOR THEOREM
{
	// the problem is in partial eval, i need to reevaluate how i want that to work at all
	// we should just call eval and let the chips lie where they may
	// you just happen to eval from that point
	// eval shouldn't come across any regressive products at that point in the tree, so it SHOULD be safe!!!

	test( dimension = 4, basis = string.["e1","e2","e3","e4"], expression = "(e2^e3)R(e1^e3^e4)" );	
}

// THIS IS THE ORIGINAL TESTING OF THE TOP-LEVEL FUNCTIONS
	test( dimension = 5, basis = string.["e1","e2","e3","e4","e5"], expression = "(e1^e2^e3)+(e2^e3^e4)+(e1^e4^e5)" );

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3^3" ); // the sum might be broken, that's okay though
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^e2" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3+3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3+3+3" );
	
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e2^e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^(e1+e2)" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(e1+e2)^(e1+e2)" );

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1 + e2 + e1" );

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3+e1+3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^(e1+e2+e3)" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(e1+e2)^(e1+e2+e3)" );

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "-3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3^5" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "-3^-5" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "3^e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "-3^e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^e2" );	
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(e1^e2)" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(e1^e2)^e3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^e2^e3" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(3^e1)^(4^e2)" );	
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1+e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1-e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1+e2+e1" ); 

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1-2^e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^e2+e1" );
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1^(e1+e2)" ); 
	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "(e1+e3)^(e1+e2)" ); 

	return;
}

#scope_file
#import "Basic";
