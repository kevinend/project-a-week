#scope_export

/*
	The parser should always return a valid AST. It doesn't return an AST or Errors, it will return an AST with errors.
	If the tree has multiple errors only currently report the first error, this can be changed by removing the flag
	check to allow for cascading errors.

	For example, if all the following identifiers were undefined then the expression
	L1 + L2 + L3
	would first report an unresolved identifier for L1; once that was fixed the next execution
	would report an unresolved identifier for L2
	and so on...

	This is the first attempt at having an 'error pass' so it will probably be buggy and need cleaned up!

	PUT COMMENT IN HERE ABOUT HOW THE TREE IS STRUCTURED WHY WE TEST FIRST
	AST's nodes are ordered based on operator precedence, so an exterior product occurs BEFORE a sum
	in
*/

syntax_errors :: ( node: *Ast_Node ) -> bool {

	found_error: bool = false;
	if node.kind == .Error {
		error := cast(*Error_Node)node;
		print( "Syntax error occurred. %\n", error.message );
		found_error = true;
	}
	else
	if node.kind == .Unary_Op {
		unary_operator := cast(*Unary_Operator_Node)node;

		if unary_operator.operand.kind == .Error {
			error := cast(*Error_Node)unary_operator.operand;
			print( "Syntax error occurred following unary operator. %\n", error.message );
			found_error = true;
		}

		if unary_operator.operand.kind == .Unary_Op || unary_operator.operand.kind == .Binary_Op {
			found_error = syntax_errors( unary_operator.operand );
		}
	}
	else
	if node.kind == .Binary_Op {
		binary_operator := cast(*Binary_Operator_Node)node;

		if binary_operator.left.kind == .Error {
			error := cast(*Error_Node)binary_operator.left;
			if binary_operator.op == {
				case .Assign; 			  { print( "Syntax error in left argument for assignment statement. %", error.message ); }
				case .Sum;    			  { print( "Syntax error in left argument of sum. %", error.message ); }
				case .Difference;         { print( "Syntax error in left argument of difference. %", error.message ); }
				case .Exterior_Product;   { print( "Syntax error in left argument of exterior product. %", error.message ); }
				case .Regressive_Product; { print( "Syntax error in left argument of regressive product. %", error.message ); }
				case .Interior_Product;   { print( "Syntax error in left argument of interior product. %", error.message ); }
			}

			found_error = true;
		}

		if binary_operator.right.kind == .Error {
			error := cast(*Error_Node)binary_operator.right;
			if binary_operator.op == {
				case .Assign; 			  { print( "Syntax error in right argument for assignment statement. %", error.message ); }
				case .Sum;    			  { print( "Syntax error in right argument of sum. %", error.message ); }
				case .Difference;         { print( "Syntax error in right argument of difference. %", error.message ); }
				case .Exterior_Product;   { print( "Syntax error in right argument of exterior product. %", error.message ); }
				case .Regressive_Product; { print( "Syntax error in right argument of regressive product. %", error.message ); }
				case .Interior_Product;   { print( "Syntax error in right argument of interior product. %", error.message ); }
			}

			found_error = true;
		}

		if binary_operator.left.kind == .Unary_Op || binary_operator.left.kind == .Binary_Op {
			found_error = syntax_errors( binary_operator.left );
		}
		
		if binary_operator.right.kind == .Unary_Op || binary_operator.right.kind == .Binary_Op {
			found_error = syntax_errors( binary_operator.right );
		}	
	}

	return found_error;
}

#scope_file
