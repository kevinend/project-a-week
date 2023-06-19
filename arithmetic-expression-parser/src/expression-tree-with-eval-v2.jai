/*
	Parses simple mathematical expressions into an 'expression' tree.
	Nodes in the tree represent different parts of the mathematical expression.
	The parsing routines correctly handle operator precedence.

	BNF for an expression.
	expression ::= [-]<term> [("+","-") <term>]...
	term   	   ::= <factor>  [("*","/") <factor]...
	factor     ::= <number>  | "(" <expression> ")"
*/

#scope_export 

main :: () {

{
	expression: string = "3+4+5";
	
	tree: *Expression_Node;
	tree = parse_expression( expression );
	print_in_order_traversal( tree );

	value := eval( tree );
	print( "Value is %\n", value );
};

{
	expression: string = "3*4+5"; // we lost the + 5 on here, shit
	
	tree: *Expression_Node;
	tree = parse_expression( expression );
	print_in_order_traversal( tree );

	value := eval( tree );
	print( "Value is %\n", value );
};

{
	expression: string = "(3+4)*7-5"; 
	
	tree: *Expression_Node;
	tree = parse_expression( expression );
	print_in_order_traversal( tree );

	value := eval( tree );
	print( "Value is %\n", value );
}

{
	expression: string = "3*((7+1)/4) + 17-5"; 
	
	tree: *Expression_Node;
	tree = parse_expression( expression );
	print_in_order_traversal( tree );

	value := eval( tree );
	print( "Value is %\n", value );
}

{
	expression: string = "3 + 4 + 5"; 

	tree: *Expression_Node;
	tree = parse_expression( expression );
	print_in_order_traversal( tree );

	value := eval( tree );
	print( "Value is %\n", value );
}

	return;
}

#scope_file
#import "Basic";

Node_Kind :: enum {
	Constant;
	Binary_Operator;
}

Expression_Node :: struct {
	kind:  Node_Kind;
	value: float;
}

Constant_Node :: struct {
	using _kind: Expression_Node;

	is_negative: bool;
}

Binary_Operator_Node :: struct {
	using _kind: Expression_Node;

	left: 	   *Expression_Node;
	operation:  u8;
	right: 	   *Expression_Node;
}

skip_spaces :: ( s: string ) -> ( cursor: string ) {
	cursor := s;
	while cursor && (cursor[0] == #char " " || cursor[0] == 9) {
		advance( *cursor, 1 );
	}
	
	return cursor;
}

parse_expression :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	term: *Expression_Node;

	cursor := skip_spaces( expression );

	term_is_negative: bool = false;
	if cursor[0] == #char "-" {
		term_is_negative = true;
		advance( *cursor, 1 );
	}

	term, cursor = parse_term( cursor );
	if term_is_negative {
		// Note: Working on grassmann parser, this might not work as written!!!
		// term is a constant node, cast to it and set the 'is_negative' property
		constant_node := cast(*Constant_Node)term;
		constant_node.is_negative = true;
		// don't return here
	}

	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "+" || cursor[0] == #char "-") {

		operation:  u8;
		next_term: *Expression_Node;

		operation = cursor[0];
		advance( *cursor, 1 );
		
		next_term, cursor = parse_term( cursor );

		node 		  := New(Binary_Operator_Node);
		node.kind 	   = Node_Kind.Binary_Operator;
		node.left 	   = term;
		node.operation = operation;
		node.right     = next_term;

		term = cast(*Expression_Node)node;
	}

	return term, cursor;
}

parse_term :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	factor: *Expression_Node;
	cursor := skip_spaces( expression );
	
	factor, cursor = parse_factor( cursor );

	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "*" || cursor[0] == #char "/") {
	
		operation: u8;
		next_factor: *Expression_Node;

		operation = cursor[0];
		advance( *cursor, 1 );

		next_factor, cursor = parse_factor( cursor );

		node			:= New(Binary_Operator_Node);
		node.kind		 = Node_Kind.Binary_Operator;
		node.left		 = factor;
		node.operation	 = operation;
		node.right       = next_factor;

		factor = cast(*Expression_Node)node;
	}

	return factor, cursor;
}

parse_factor :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {
	
	cursor := skip_spaces( expression );

	node := New(Expression_Node);

	if is_digit( cursor[0] ) {
		success: bool;
		value: float;
		value, success, cursor = string_to_float( cursor );

		node.kind  = Node_Kind.Constant;
		node.value = value;	
	}
	else
	if cursor[0] == #char "(" {
		advance( *cursor, 1 );
		cursor = skip_spaces( cursor );
		
		node, cursor = parse_expression( cursor );
		cursor = skip_spaces( cursor );

		if cursor[0] == #char ")" {
			advance( *cursor, 1 );
		}
		else {
			print( "Error, unable to parse the expression, expected a terminating right paren ')'\n" );
			exit( -1 );
		}
	}
	else {
		print( "Error, unable to parse expression, expected either a number or the start of an expression '('\n" );
		exit( -2 );
	}

	return node, cursor;
}

print_in_order_traversal :: ( node: *Expression_Node ) {
	
	if node.kind == Node_Kind.Constant {
		print( "Node is a constant. " );
		constant := cast(*Constant_Node)node;
		print( "Value of the node is %\n", constant.value );
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		print( "Node is a binary operator. Operator is " );
		binary_op := cast(*Binary_Operator_Node)node;

		if binary_op.operation == {
			case #char "+"; { print( "+\n" ); }
			case #char "-"; { print( "-\n" ); }
			case #char "*"; { print( "*\n" ); }
			case #char "/"; { print( "/\n" ); }
		}

		print_in_order_traversal( binary_op.left );
		print_in_order_traversal( binary_op.right );
	}
}

eval :: ( node: *Expression_Node ) -> ( value: float ) {

	value: float;
	if node.kind == Node_Kind.Constant {
		// constant node's value is written directly on the expression node
		value = node.value;
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		binary_op := cast(*Binary_Operator_Node)node;
		
		left_\value := eval( binary_op.left );
		right_value := eval( binary_op.right );

		if binary_op.operation == {
			case #char "+"; { value = left_value + right_value; }
			case #char "-"; { value = left_value - right_value; }
			case #char "*"; { value = left_value * right_value; }
			case #char "/"; { value = left_value / right_value; }
		}
	}

	return value;
}
