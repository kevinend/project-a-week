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

// this is pre-order traversal, you got the name mixed up you bone
print_in_order_traversal :: ( node: *Expression_Node ) {

	if node.kind == Node_Kind.K_Vector {
		print( "Node is a k-vector. " );
		k_vector := cast(*K_Vector_Node)node;

		builder: String_Builder;
		for i: 0..basis_one_elements.count-1 {
			if k_vector.basis_components & (1 << i) {
				append( *builder, basis_one_elements[i] );
				if i < k_vector.grade-1 {
					// in an expression like e1^e2 there is one operator less than the grade of the element
					// this holds for longer expressions like e1^e2^e3^e4 which has three ^ and a grade of 4, etc.
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

expand_full :: ( node: *Expression_Node ) {
	
	// this is not the most performant approach but we can basically 'expand' one step at a time
	// or use this procedure to fully expand the tree
	// want this separate from simplify, so we can view the tree right before evaluation

	expanded: bool = expand( node );
	while expanded {
		expanded = expand( node );
	}

	return;
}

// need to consider this full-expansion business
expand :: ( node: *Expression_Node ) -> ( expanded: bool ) {

	// NOTE: This case doesn't exist yet but make sure ~(e1^e2) ^ (e1+e2) is reordered correctly 

	expanded_lvalue: bool = false;
	expanded_rvalue: bool = false;


	if node.kind == Node_Kind.Binary_Operator {
		// recurse into the tree, need to expand leaf nodes first if they are themselves binary operators
		binary_operator_node := cast(*Binary_Operator_Node)node;
		expanded_lvalue = expand( binary_operator_node.left_operand );
		expanded_rvalue = expand( binary_operator_node.right_operand );
	}
	else {
		// don't expand K_Vector or Unary_Operator nodes
		return false;
	}

	// recursion terminates above on non-binary operator nodes, so this is definitely a binary operator
	binary_operator_node := cast(*Binary_Operator_Node)node;
	if binary_operator_node.operation == #char "^" {
		if binary_operator_node.left_operand.kind == Node_Kind.Binary_Operator {
			left_operator := cast(*Binary_Operator_Node)binary_operator_node.left_operand;
			if left_operator.operation == #char "+" || left_operator.operation == #char "-" {
				
				// RIGHT DISTRIBUTIVITY - distribute the argument on the right to the argument on the left
				
				distributive_operator    := binary_operator_node.operation;
				distribute_over_operator := left_operator.operation;

				new_\left_node := New(Binary_Operator_Node);
				new_right_node := New(Binary_Operator_Node);

				new_left_node.kind           = Node_Kind.Binary_Operator;
				new_left_node.operation 	 = distributive_operator;
				new_left_node.left_operand   = left_operator.left_operand;
				new_left_node.right_operand  = binary_operator_node.right_operand;

				new_right_node.kind          = Node_Kind.Binary_Operator;
				new_right_node.operation 	 = distributive_operator;
				new_right_node.left_operand  = left_operator.right_operand;
				new_right_node.right_operand = binary_operator_node.right_operand;
	
				binary_operator_node.operation     = distribute_over_operator; // + or -
				binary_operator_node.left\_operand = cast(*Expression_Node)new_left_node;
				binary_operator_node.right_operand = cast(*Expression_Node)new_right_node;	

				expanded_rvalue = true;
			}
		}
		else
		if binary_operator_node.right_operand.kind == Node_Kind.Binary_Operator {
			right_operator := cast(*Binary_Operator_Node)binary_operator_node.right_operand;
			if right_operator.operation == #char "+" || right_operator.operation == #char "-" {

				// LEFT DISTRIBUTIVITY - distribute the argument on the left to the argument on the right
				
				distributive_operator    := binary_operator_node.operation;
				distribute_over_operator := right_operator.operation;

				new_\left_node := New(Binary_Operator_Node);
				new_right_node := New(Binary_Operator_Node);

				new_left_node.kind           = Node_Kind.Binary_Operator;
				new_left_node.operation 	 = distributive_operator;
				new_left_node.left_operand   = binary_operator_node.left_operand;
				new_left_node.right_operand  = right_operator.left_operand;

				new_right_node.kind          = Node_Kind.Binary_Operator;
				new_right_node.operation 	 = distributive_operator;
				new_right_node.left_operand  = binary_operator_node.left_operand;
				new_right_node.right_operand = right_operator.right_operand;
	
				binary_operator_node.operation     = distribute_over_operator; // + or -
				binary_operator_node.left\_operand = cast(*Expression_Node)new_left_node;
				binary_operator_node.right_operand = cast(*Expression_Node)new_right_node;

				expanded_lvalue = true;
			}
		}
	}

	return expanded_lvalue || expanded_rvalue;
}



simplify :: ( node: *Expression_Node ) -> ( result: *Expression_Node ) {

	result: *Expression_Node;
	if node.kind == Node_Kind.K_Vector {
		result = node;
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		binary_operator_node := cast(*Binary_Operator_Node)node;
		left\_node:  *Expression_Node;
		right\_node: *Expression_Node;

		left\_node = simplify( binary_operator_node.left_operand );
		right_node = simplify( binary_operator_node.right_operand );

		if left_node.kind == Node_Kind.K_Vector && right_node.kind == Node_Kind.K_Vector {
			lvalue := cast(*K_Vector_Node)left\_node; 
			rvalue := cast(*K_Vector_Node)right_node;

			k_vector := New(K_Vector_Node);
			k_vector.kind = Node_Kind.K_Vector;
	
			if binary_operator_node.operation == #char "^" {
			
				if (lvalue.grade + rvalue.grade) > dimension {
					k_vector.grade = 0;
					k_vector.coefficient = 0;
				}
				else
				if lvalue.grade == 0 {
					k_vector.grade       	  = rvalue.grade;
					k_vector.basis_components = rvalue.basis_components;
					k_vector.coefficient 	  = lvalue.coefficient * rvalue.coefficient;	
				}
				else
				if rvalue.grade == 0 {
					k_vector.grade       = lvalue.grade;
					k_vector.coefficient = lvalue.coefficient * rvalue.coefficient;
				}
				else
				if (lvalue.basis_components & rvalue.basis_components) > 0 {
					// k-vector arguments share at least one common basis vector and exterior product is zero
					k_vector.grade = 0;
					k_vector.coefficient = 0;	
				}
				else {
					k_vector.grade 		 	  = lvalue.grade + rvalue.grade;
					k_vector.coefficient 	  = lvalue.coefficient * rvalue.coefficient;
					k_vector.basis_components = lvalue.basis_components | rvalue.basis_components; 
				}
			}
			else
			if binary_operator_node.operation == #char "+" {
				// becareful in here, you are comparing against zero using floats!
				// will need to introduce an epsilon at some point
				if lvalue.coefficient == 0 && rvalue.coefficient == 0 {
					// result is zero
					k_vector.grade = 0;
					k_vector.basis_components = 0;
					k_vector.coefficient      = 0;
				}
				else
				if lvalue.coefficient == 0 && rvalue.coefficient != 0 {
					k_vector.grade 			  = rvalue.grade;
					k_vector.basis_components = rvalue.basis_components;
					k_vector.coefficient      = rvalue.coefficient;

				}
				else
				if lvalue.coefficient != 0 && rvalue.coefficient == 0 {
					k_vector.grade 			  = lvalue.grade;
					k_vector.basis_components = lvalue.basis_components;
					k_vector.coefficient      = lvalue.coefficient;
				}
				else if lvalue.basis_components == rvalue.basis_components {
					sum := lvalue.coefficient + rvalue.coefficient;
					if sum == 0 { // this might be dangerous because of floating point rounding errors!
						k_vector.grade 			  = 0;
						k_vector.basis_components = 0;
						k_vector.coefficient      = sum;
					}
					else {
						k_vector.grade 			  = lvalue.grade;
						k_vector.basis_components = lvalue.basis_components;
						k_vector.coefficient      = sum;
					}
				}
			}
			else
			if binary_operator_node.operation == #char "-" && lvalue.basis_components == rvalue.basis_components {
				difference := lvalue.coefficient - rvalue.coefficient;
				if difference == 0 { // this might be dangerous because of floating point rounding errors!
					k_vector.grade 			  = 0;
					k_vector.basis_components = 0;
					k_vector.coefficient      = difference;
				}
				else {
					k_vector.grade 			  = lvalue.grade;
					k_vector.basis_components = lvalue.basis_components;
					k_vector.coefficient      = difference;
				}
			}
			
			result = cast(*Expression_Node)k_vector;
		}
	}

	return result;
}

main :: () {

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "3";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded := expand( ast );
	print( "Expanded == %\n", expanded );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast ); // should just return the node
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "3^5";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded := expand( ast );
	print( "Expanded == %\n", expanded );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast ); // should multiply the two 0-vectors together, cool looks like it worked
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "3^e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded := expand( ast );
	print( "Expanded == %\n", expanded );

	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast ); // should be a single node with coefficient 3 and basis component e1
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^e1";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^e2";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "(3^e1)^(4^e2)"; 
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1-e1"; // if we sum or sub to zero it should just display zero
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1-2^e1"; // if we sum or sub to zero it should just display zero
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result );
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^(e1+e2)"; 
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded := expand( ast );
	print( "Expanded == %\n", expanded );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result ); // i don't think we handle the + operator in the expression
	print( "\n" );
};

{
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "(e1+e3)^(e1+e2)"; 
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded := expand( ast );
	print( "Expanded == %\n", expanded );
	print_in_order_traversal( ast );
	print( "\n" );

	expanded = expand( ast );
	print( "Expanded == %\n", expanded );
	print_in_order_traversal( ast );
	print( "\n" );

	result := simplify( ast );
	print_in_order_traversal( result ); // i don't think we handle the + operator in the expression
	print( "\n" );
};

// so our simplify wants to return a single node but we may need to return a tree
// like in this case we need the final tree to be something like 

/*
     +
   ^   	+
 e1 e2 ^   ^
     e3 e1 e3 e2 and that is the simplified verison of the tree

*/





/*
{
	// distributive property single vector over sum
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "e1^(e1+e2)";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );
};

{
	// distributive property, sum of vectors
	set_dimension( N = 3 );
	set_basis_one_elements( string.["e1","e2","e3"] );

	expression: string = "(e1+e3)^(e1+e2)";
	ast: *Expression_Node = parse_expression( expression );
	print_in_order_traversal( ast );
	print( "\n" );

	// (e1+e3)^(e1) + (e1+e3)^e2
	//     	 +
	//   ^   	 ^
	//  + e1    +  e2
	//e1 e3   e1 e3


	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );

	// if we exapnd again we should get
	// (e1^e1)+(e1^e3) + (e1^e2) + (e3^e2)
	//
	// as a tree
	//
	//          	+
	//        + 		  +
	//    ^     ^      ^	  ^
	//  e1 e1  e1 e3 e1 e2  e3 e2
	expand( ast );
	print_in_order_traversal( ast );
	print( "\n" );
};
*/

/*
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

*/
	
	return;
}

#scope_file
#import "Basic";
