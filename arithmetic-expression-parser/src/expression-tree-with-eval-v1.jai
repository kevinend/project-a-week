/*
	Parses simple mathematical expressions into an 'expression' tree.
	Nodes in the tree represent different parts of the mathematical expression.
	The parsing routines do not correctly handle operator precedence, need to use parens.

	BNF for an expression.
	expression ::= <number> | "(" <expression> <operator> <expression> ")"
	operator   ::= "+" | "-" | "*" | "/"
*/

#scope_export 
 
main :: () {

// Testing the parsing and eval procedures.
// Parse the expression into an 'expression' tree then evaluate the tree.
{
	expression: string = "3";

	tree: *Expression_Node;
	tree = parse_expression( expression );

	print_in_order_traversal( tree );

	value: float = eval( tree );
	print( "Eval result == %\n", value );
};

{
	print( "\n" );
	expression: string = "(3+4)";

	tree: *Expression_Node;
	tree = parse_expression( expression );

	print_in_order_traversal( tree );

	value: float = eval( tree );
	print( "Eval result == %\n", value );
};

{
	print( "\n" );
	expression: string = "((3*4)+2)";

	tree: *Expression_Node;
	tree = parse_expression( expression );

	print_in_order_traversal( tree );

	value: float = eval( tree );
	print( "Eval result == %\n", value );
}

}

#scope_file 
#import "Basic";

Node_Kind :: enum {
	Constant;
	Binary_Operator;
}

Expression_Node :: struct {
	kind: Node_Kind;
}

Constant_Node :: struct {
	using _kind: Expression_Node;
	value: float;
}

Binary_Operator_Node :: struct {
	using _kind: Expression_Node;

	value: float;

	left:  *Expression_Node;
	op:     u8;
	right: *Expression_Node;
}

skip_spaces :: ( s: string ) -> ( cursor: string ) {
	cursor := s;
	while cursor && (cursor[0] == #char " " || cursor[0] == 9) { // horizontal tab
		advance( *cursor, 1 );
	}

	return cursor;
}

parse_expression :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	if is_digit( cursor[0] ) {
		success: bool;
		value: float;
		value, success, cursor = string_to_float( cursor );

		node := New(Constant_Node);
		node.kind  = Node_Kind.Constant;
		node.value = value;

		return cast(*Expression_Node)node, cursor;
	}
	else 
	if cursor[0] == #char "(" {
		advance( *cursor, 1 );
		cursor = skip_spaces( cursor );

		left:  *Expression_Node;
		right: *Expression_Node;
		op:     u8;

		left,  cursor = parse_expression( cursor );
		op,    cursor = parse_operator( cursor );
		right, cursor = parse_expression( cursor );

		cursor = skip_spaces( cursor );
		if cursor[0] == #char ")" {
			advance( *cursor, 1 );

			node := New(Binary_Operator_Node);
			node.kind  = Node_Kind.Binary_Operator;
			node.left  = left;
			node.op    = op;
			node.right = right;

			return cast(*Expression_Node)node, cursor;
		}
		else {
			print( "Error, expected closing paren to terminate the expression" );
			exit( -1 );
		}
	}
	else {
		print( "Error, unable to parse the expression, expected either a number of opening paren (\n" );
		exit( -1 );
	}
}

parse_operator :: ( expression: string ) -> ( op: u8, cursor: string ) {
	
	cursor := skip_spaces( expression );
	c := cursor[0];
	if c == #char "+" || c == #char "-" || c == #char "*" || c == #char "/" {
		op: u8 = c;
		advance( *cursor, 1 );
		return op, cursor;
	}
	else {
		print( "Error, expected an operator for the expression\n" );
		exit( -1 );
	}
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

		if binary_op.op == {
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
		constant := cast(*Constant_Node)node;
		value = constant.value;
	}
	else
	if node.kind == Node_Kind.Binary_Operator {
		binary_op := cast(*Binary_Operator_Node)node;
		
		left_\value := eval( binary_op.left );
		right_value := eval( binary_op.right );

		if binary_op.op == {
			case #char "+"; { value = left_value + right_value; }
			case #char "-"; { value = left_value - right_value; }
			case #char "*"; { value = left_value * right_value; }
			case #char "/"; { value = left_value / right_value; }
		}
	}

	return value;
}
