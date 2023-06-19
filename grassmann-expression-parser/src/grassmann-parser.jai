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
		exterior-factor ::= ["-"] <number> | <basis-one-element> | "(" <expression> ")"

	At the end of the week I did not make it as far as I hoped on this, the way she goes.
	The code compiles and runs without errors for the examples in main but there is a lot more functionality
	I intend to add to this one. Some of that functionality is listed below.

	TODO:
	--Generate the span and cospans for elements (this would be super useful for expanding products)
	--Complement (left and right)
	--'Expand' functionality so that we can take the binary operator and return a new node that it replaces it, like two vectors becomes a bivector
	   Turn expressions of form e1^(e1+e2) into e1^e1 + e1^e2	
	--'Simplify'/Eval that does the expansion recursively to give us the final form of the expression 
	   Turn expressions of form e1^e1 + e1^e2 into 0 + e1^e2 = e1^e2 into a single node
	--How do you free memory allocated to the string or string builder types built into the language?

	NOTE: I had this little 'expansion' loop idea the other night, not sure if we will use it or not but leaving it in for now
  	while ast, fully_expanded = expand( ast ) {
  		expand( ast ); // what expand actually does is kind of up in the air, tries to do the distributive property, or rewrite interior products as a sequence of products (this might be hard asf, but we can do it)
  	}
  	eval( ast );
*/

#scope_export

dimension: u8 = 0;
basis_one_elements: [..] string;

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
	grade: u8;
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

print_in_order_traversal :: ( node: *Expression_Node ) {

	if node.kind == Node_Kind.K_Vector {
		print( "Node is a k-vector. " );
		k_vector := cast(*K_Vector_Node)node;

		builder: String_Builder;
		for i: 0..basis_one_elements.count-1 {
			if k_vector.basis_components & (1 << i) {
				append( *builder, basis_one_elements[i] );
				if k_vector.grade > 1 {
					append( *builder, "^" );
				}
			}
		}

		// no idea how to free the builder or final string in this language
		components: string = builder_to_string( *builder );

		if k_vector.grade > 0 {
			print( "Grade: %, Scalar Coefficient: %, Components: %\n", k_vector.grade, k_vector.coefficient, components );
		}
		else {
			print( "Grade: %, Scalar Coefficient: %\n", k_vector.grade, k_vector.coefficient );
		}
	}
	else 
	if node.kind == Node_Kind.Binary_Operator {
		print( "Node is a binary_operator. Operator is " );
		binary_operator := cast(*Binary_Operator_Node)node;

		if binary_operator.operation == {
			case #char "^"; { print( "^\n" ); }
			case #char "+"; { print( "+\n" ); }
			case #char "-"; { print( "-\n" ); }
			case; print( "Unhandled operator case in print procedure.\n" );
		}

		print_in_order_traversal( binary_operator.left_operand );
		print_in_order_traversal( binary_operator.right_operand );
	}

	return;
}

main :: () {

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e3";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "-3";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "-3^e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "-3^-1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};


{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );
	
	expression: string = "3";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "3^e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^e2";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^e2^e3";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "(e1^e2)";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^(e1^e2)";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1+e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^e2+e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1+e2^e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1+e2+e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );
};
	
	return;
}

#scope_file
#import "Basic";
