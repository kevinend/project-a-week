/*
	This file implements a simple parser of Grassmann (exterior) algebra expressions.
	The structure of expressions handled by the parser are expressed as Backus-Naur Forms
	(BNF).

	As mentioned in the readme, Grassmann's products, especially the interior product of
	different graded elements is recursive in nature and cumbersome to expand by hand.
	This parser is written to allow an expansion phase to see the products fully expanded
	from an expression tree and then evaluated to return the final result.

	BNF for Grassmann expressions:

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^") <exterior-factor>]...
		exterior-factor ::= ["-"] ( <basis-one-element> | <number> |  | "(" <expression> ")" )

	At the end of the week I did not make it as far as I hoped on this, the way she goes.
	The code compiles and runs without errors for the examples in main but there is a lot more functionality
	I intend to add to this one. Some of that functionality is listed below.

	NOTE:
	  Multiplication of basis elements by scalars is a tad clumsy.
	  You must specify multiplication using the exterior product. 3^e1 and not 3e1.
	  This isn't obvious until you try to parse the number 3 from 3e1 and use something like string-to-float
	  which e represents the exponent so it doesn't get read as a number with the additional e1 after it. It
	  gets read as 3e1 with the e interpreted as an exponent.

	  If your basis is not e1,e2,e3..eN then you could handle this differently but I don't want to restrict that
	  at this time so we are left with explicit exterior products.

	TODO:
	-- SORT THE LINEAR LIST AND DO SUMS THAT WAY INSTEAD OF ADDING UP
	--The way we encode the basis components currently is going to generate a bunch of incorrect signs
	  If we wedge e1^e3 then this is interpreted as e1^e3 but we need e3^e1 to be -e1^e3 if that is the order
	  of the call, it is too hot out here to even think, heading back inside
	--That print failed us in that one case and I'm not sure what went wrong on the last test case
	--Generate the span and cospans for elements (this would be super useful for expanding products)
	--Complement (left and right)
	--'Expand' functionality so that we can take the binary operator and return a new node that it replaces it, like two vectors becomes a bivector
	   Turn expressions of form e1^(e1+e2) into e1^e1 + e1^e2	
	--How do you free memory allocated to the string or string builder types built into the language?

	-- could do the flat list as well
	   e1 e1 e1 e2, so you sort then you sum
	   e1
	   e1, when it doesn't look like you, you add it to the list then you determine the next index, could do that as well
*/

#scope_export

dimension: u8 = 0;
basis_one_elements: [..] string;
test_case: int = 0;

set_dimension :: ( N: u8 ) {

	assert( N > 0 );

	dimension = N;
	return;
}

set_basis_one_elements :: ( symbols: [] string ) {

	assert( symbols.count == dimension );
	assert( symbols.count > 0 );

	if basis_one_elements.count > 0 {
		array_reset( *basis_one_elements );
	}

	// copy the symbols over into our global symbol/basis elements table
	for symbol: symbols {
		array_add(*basis_one_elements, symbol );
	}

	return;
}

Node_Kind :: enum {
	K_Vector;
	Unary_Operator;  // complement (lcomp, rcomp)
	Binary_Operator; // exterior, regressive, interior
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

Unary_Operator_Node :: struct {

	// left and right complement
	using _instance: Expression_Node;

	operation: string;
	operand:   *Expression_Node;
}

Binary_Operator_Node :: struct {

	// exterior, regressive, and interior products, +, -, *, / (division just handled for scalars now even though grassman division is supported)
	using _instance: Expression_Node;

	operation: u8;
	left_\operand: *Expression_Node;
	right_operand: *Expression_Node;
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

	while cursor && cursor[0] == #char "^" {
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

clone_node :: ( node: *Expression_Node ) -> ( cloned_node: *Expression_Node ) {
		
	cloned_node: *Expression_Node;
	if node.kind == Node_Kind.K_Vector {
		k_vector_node := cast(*K_Vector_Node)node;

		new_node := New(K_Vector_Node);

		new_node.kind = Node_Kind.K_Vector;
		new_node.grade            = k_vector_node.grade;
		new_node.coefficient      = k_vector_node.coefficient;
		new_node.basis_components = k_vector_node.basis_components;

		cloned_node = cast(*Expression_Node)new_node;
	}
	else
	if node.kind == Node_Kind.Unary_Operator {
		unary_operator_node := cast(*Unary_Operator_Node)node;

		new_node := New(Unary_Operator_Node);
		new_node.kind = Node_Kind.Unary_Operator;
		new_node.operation = unary_operator_node.operation;
		new_node.operand   = clone_node( unary_operator_node.operand );

		cloned_node = cast(*Expression_Node)new_node;
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		binary_operator_node := cast(*Binary_Operator_Node)node;		

		new_node := New(Binary_Operator_Node);
		new_node.kind = Node_Kind.Binary_Operator;
		new_node.operation = binary_operator_node.operation;
		new_node.left\_operand = clone_node( binary_operator_node.left\_operand );  
		new_node.right_operand = clone_node( binary_operator_node.right_operand );

		cloned_node = cast(*Expression_Node)new_node;
	}
	else {
		print( "Unrecognized node kind found when cloning nodes\n" );
		cloned_node = null;
	}

	return cloned_node;
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
	else
	{
		// Determine whether we need to distribute the grassmann products over any sums or differences.
		// The boolean values are introduced to reduce the level of nesting that happens if you don't 
		// do it that way (it was very hard to visually parse and a lot of duplicate 'clone the current node because
		// we don't need to distribute')

		binary_operator_node := cast(*Binary_Operator_Node)node;
		left\_operator: *Binary_Operator_Node = null;
		right_operator: *Binary_Operator_Node = null;

		distribute_over_left\_operand := false;
		distribute_over_right_operand := false;

		if binary_operator_node.operation == #char "^" && binary_operator_node.left_operand.kind == Node_Kind.Binary_Operator {
			left_operator = cast(*Binary_Operator_Node)binary_operator_node.left_operand;
			if left_operator.operation == #char "+" || left_operator.operation == #char "-" {
				distribute_over_left_operand = true;
			}
		}
		else	
		if binary_operator_node.operation == #char "^" && binary_operator_node.right_operand.kind == Node_Kind.Binary_Operator {
			right_operator = cast(*Binary_Operator_Node)binary_operator_node.right_operand;
			if right_operator.operation == #char "+" || right_operator.operation == #char "-" {
				distribute_over_right_operand = true;
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

filter :: ( basis_elements: [..] K_Vector_Node ) -> ( k_vectors: [..] K_Vector_Node ) {
	
	k_vectors: [..] K_Vector_Node;
	for element: basis_elements {
		if element.coefficient != 0 {
			array_add( *k_vectors, element );
		}
	}
	return k_vectors;
}

sort :: ( k_vectors: *[..] K_Vector_Node ) {

	// bubble-sort for k-vectors
	swap: bool = false;
	for _: 0..k_vectors.count-1 {
		for i: 0..k_vectors.count-2 {
			if (<<k_vectors)[i+1].basis_components < (<<k_vectors)[i].basis_components {
				temp     	  	  := (<<k_vectors)[i];
				(<<k_vectors)[i]   = (<<k_vectors)[i+1];
				(<<k_vectors)[i+1] = temp;

				swap = true;
			}
		}

		if !swap { return; }
		swap = false;
	}
	
	return;
}

flatten :: ( subtree: *Expression_Node ) -> ( operators: Queue(u8), elements: Queue(K_Vector_Node) ) {
	
	// In-order traversal of the distributed expression tree.
	// Once we have the flattened list of operators (sums/differences/grassmann products) and elements (1-vector/basis 1 elements)
	// we need to evaluate the possible cases of our tree.
	//
	
	flatten_helper :: ( node: *Expression_Node, operators: *Queue(u8), elements: *Queue(K_Vector_Node) ) {

		success: bool;
		
		if node.kind == Node_Kind.K_Vector {
			k_vector_node := cast(*K_Vector_Node)node;
			
			k_vector: K_Vector_Node;
			k_vector.kind 		 	  = Node_Kind.K_Vector;
			k_vector.coefficient 	  = k_vector_node.coefficient;
			k_vector.grade       	  = k_vector_node.grade;
			k_vector.basis_components = k_vector_node.basis_components;

			success = enqueue( elements, k_vector );
		}
		else
		if node.kind == Node_Kind.Binary_Operator {
			binary_operator_node := cast(*Binary_Operator_Node)node;
			flatten_helper( binary_operator_node.left\_operand, operators, elements );

			success = enqueue( operators, binary_operator_node.operation );

			flatten_helper( binary_operator_node.right_operand, operators, elements );
		}
	}

	operators := queue_init( max_num_elements = 100, u8 );
	elements  := queue_init( max_num_elements = 100, K_Vector_Node );

	flatten_helper( subtree, *operators, *elements );

	return operators, elements;
}

eval :: ( operators: *Queue(u8), elements: *Queue(K_Vector_Node) ) -> ( k_vectors: [..] K_Vector_Node ) {

	copy_elements :: ( k_vectors: *Queue(K_Vector_Node) ) -> ( copies: [..] K_Vector_Node ) {

		// May be unnecessary. Seemed cleaner to return only the final results and not the queue from the eval proc.
		copies: [..] K_Vector_Node;

		while !is_empty( k_vectors ) {
			k_vector, success := dequeue( k_vectors );
			array_add( *copies, k_vector );
		}

		return copies;
	}

	exterior_product :: ( lvalue: K_Vector_Node, rvalue: K_Vector_Node ) -> ( result: K_Vector_Node ) {

		result: K_Vector_Node;
		result.kind = Node_Kind.K_Vector;

		if (lvalue.grade + rvalue.grade) > dimension {
			result.grade = 0;
			result.basis_components = 0;
			result.coefficient = 0;
		}
		else
		if lvalue.grade == 0 || rvalue.grade == 0 {
			result.grade       	    = lvalue.grade + rvalue.grade;
			result.basis_components = lvalue.basis_components | rvalue.basis_components;
			result.coefficient 	    = lvalue.coefficient * rvalue.coefficient;	
		}
		else
		if (lvalue.basis_components & rvalue.basis_components) > 0 {
			// k-vector arguments share at least one common basis vector and exterior product is zero
			result.grade 			= 0;
			result.basis_components = 0;
			result.coefficient 	    = 0;	
		}
		else {
			// Take the exterior product between the elements and handle the anticommutativity property of the grassmann product.
			// For every bit (basis one element) in the LEFT argument
			//	Start at right-most bit in the LEFT arg
			//  Find it's place in the RIGHT arg and set that bit.
			//	Anytime you cross over a bit in the RIGHT that is set, then you have swapped the elements.
			// Rinse and repeat for every other bit in the LEFT.
			//
			// If after completing this process we have swapped an odd number of times then we negate the final product.

			result.grade 		  = lvalue.grade + rvalue.grade;
			result.coefficient  = lvalue.coefficient * rvalue.coefficient;

			result.basis_components = rvalue.basis_components;
			num_swaps: int = 0;
			for < i: dimension-1..0 {
				// Start from the rightmost bit in the left argument and determine which bits are set
				if lvalue.basis_components & (1 << i) {
					// Bit in the left argument is set, iterate from the leftmost bit in the right argument
					// and everytime the a bit is set increment the swap counter.
					// When we get to the location of the bit from the left in the location on the right, set the bit in the result, stop the 'swap' loop and start over with any other bits from the left.
					for j: 0..dimension-1 {
						if j == i {
							result.basis_components |= (1 << j);
							break;
						}
						else
						if result.basis_components & (1 << j) {
							num_swaps += 1;	
						}
					}
				}
			}

			if num_swaps % 2 != 0 { result.coefficient *= -1; }	
		}

		return result;
	}

	sort_queue :: ( k_vectors: *Queue(K_Vector_Node) ) {

		// People sort queues in weird ways like dequeing and enqueing repeatedly that is hard to to follow.
		// Since head and tail are in the same location and the queue is effectively an array we will just bubble sort from
		// the head index to the end (tail). Maybe this is stupid, sue me.

		swap: bool     = false;
		num_k_vectors := k_vectors.tail - k_vectors.head;
		for _: 0..num_k_vectors-1 {
			for i: 0..num_k_vectors-2 {
				if 	(<<k_vectors).entries[i+1].grade < (<<k_vectors).entries[i].grade && 
			   		(<<k_vectors).entries[i+1].basis_components < (<<k_vectors).entries[i].basis_components {

					temp     	  	          := (<<k_vectors).entries[i];
					(<<k_vectors).entries[i]   = (<<k_vectors).entries[i+1];
					(<<k_vectors).entries[i+1] = temp;

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
  	// We dequeue the element and enqueue it twice before returning it as the result.
  	// However, it does provide a nice flat structure to the code.

  	results: [..] K_Vector_Node;
  	success: bool;

 	lvalue: K_Vector_Node;
 	rvalue: K_Vector_Node;
 	op: u8;

	sum_ops := queue_init( max_num_elements = 50, u8 ); 

	// i'm not even sure we need to track the operators again? like if we have sums if we enqueue the elements then we just sum them together
	// we don't need the minus sign anymore...hmm maybe

	lvalue, success = dequeue( elements );
	while !is_empty( operators ) {
		op,     success = dequeue( operators );
		rvalue, success = dequeue( elements );
		if op == #char "^" { 
			lvalue = exterior_product( lvalue, rvalue ); 
		}
		else
		if op == #char "+" || op == #char "-" {
			if op == #char "-" { rvalue.coefficient *= -1; }
			success = enqueue( *sum_ops, #char "+" );
			success = enqueue( elements, lvalue );
			lvalue = rvalue;
		}
	}
	success = enqueue( elements, lvalue );

	//sort_queue( elements ); // THIS IS BROKEN, not sorting correctly

	lvalue, success = dequeue( elements );
	while !is_empty( sum_ops ) {
		op,     success = dequeue( *sum_ops );
		rvalue, success	= dequeue( elements );
		if lvalue.basis_components == rvalue.basis_components {
			lvalue.coefficient += rvalue.coefficient;
		}
		else {
			success = enqueue( elements, lvalue );
			lvalue = rvalue;
		}
	}
	success = enqueue( elements, lvalue );
			
	results = copy_elements( elements );

	print( "\n" );

	return results;
}


test :: ( 
	 dimension: u8
	,basis: [] string
	,expression: string
	,should_print_expression_tree_post_parsing: 	 bool = false
	,should_print_expression_tree_post_distribution: bool = false
	,should_print_expression_tree_post_grassmann_operator_eval: bool = false
) {

	test_case += 1;

	set_dimension( N = dimension );
	set_basis_one_elements( basis );

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

	operators, elements := flatten( expression_tree_post_distribution );

	results := eval( *operators, *elements );

	// print_simplified_expression()
	for result: results {
		k_vector_as_string := k_vector_to_string( *result );
		print( "%1%2 ", result.coefficient, k_vector_as_string );
	}
	print( "\n" );

	return;
}

main :: () {

	test( dimension = 3, basis = string.["e1","e2","e3"], expression = "e1-e3+e2" );

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
#import, file "./queue.jai";
