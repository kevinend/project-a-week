#scope_export

/*
	BNF for Grassmann expressions:
		statement  		::= <identifier> "=" <expression> ";"
		statement       ::= <identifier> ";"

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"].. ["<<"].. [">>"].. ( <basis-one-element> | <number> | <identifier> | "(" <expression> ")" )
		
*/

PARSER_TEMP_BUFFER_LENGTH :: 8;
Parser :: struct {

	tokens: 	  [] Token;
	token_cursor: int;

	temp_buffer: [PARSER_TEMP_BUFFER_LENGTH] Unary_Op;
	temp_cursor: int;

	ast: *Ast_Node;

	error: string; // not capturing this at the moment!
}

parser_set_tokens :: ( parser: *Parser, tokens: [] Token, num_tokens: int ) {
	
	parser.tokens 		= tokens;
	parser.tokens.count = num_tokens;

	parser.token_cursor = 0;
	parser.ast          = null;

	return;
}

parser_generate_ast :: ( parser: *Parser ) {

	// every valid user input starts with an identifier

	root: *Ast_Node;
	root = parse_statement( parser );

	parser.ast = root;
}

new_k_element_node :: ( grade: int, coefficient: int, one_element_components: s64 ) -> ( node: *K_Element_Node ){

	node := New(K_Element_Node);
	assert( node != null );

	node.kind  		 			= Node_Kind.K_Element;
	node.grade 		 			= grade;
	node.coefficient 		    = coefficient;
	node.one_element_components = one_element_components;

	return node;
}

new_identifier_node :: ( name: string ) -> ( node: *Identifier_Node ) {

	node := New(Identifier_Node);
	assert( node != null );

	node.kind = .Identifier;
	node.name = copy_string( name ); // seems wasteful, can we intern???

	return node;
}

new_literal_node :: ( value: int ) -> ( node: *Literal_Node ) {

	node := New(Literal_Node);
	assert( node != null );
	
	node.kind = Node_Kind.Literal;
	node.value = value;

	return node;
}

new_unary_operator_node :: ( op: Unary_Op, operand: *Ast_Node ) -> ( node: *Unary_Operator_Node ) {

	node := New(Unary_Operator_Node);
	assert( node != null );

	node.kind 	  = .Unary_Op;
	node.op 	  = op;
	node.operand  = operand;

	return node;
}

new_binary_operator_node :: ( op: Binary_Op, left: *Ast_Node, right: *Ast_Node ) -> ( node: *Binary_Operator_Node ) {

	node := New(Binary_Operator_Node);
	assert( node != null );

	node.kind  = .Binary_Op;
	node.op    = op;
	node.left  = left;
	node.right = right;

	return node;
}

print_ast :: ( ast: *Ast_Node ) {

	// doesn't handle every node type yet!
	print_ast_helper :: ( node: *Ast_Node, indent: int = 0 ) {

		for i: 0..indent-1 {
			print( " " );
		}

		if node == null {
			print( "There was a parse error, either expected or unexpected that led to a null node in the ast\n" );
			return;
		}

		if node.kind == {
			case .K_Element; {
				k_element := cast(*K_Element_Node)node;
				print( "K-Element(%)\n", k_element.one_element_components ); 
			}
			case .Literal; {
				print( "Number/Literal\n" );
			}
			case .Identifier; { 
				print( "Identifier\n" ); 
			}
			case .Binary_Op; { 
				binary_operator := cast(*Binary_Operator_Node)node;
				print( "Binary Operator " );
				if binary_operator.op == {
					case .Assign;             { print( "(=)\n");  }
					case .Sum; 		  		  { print( "(+)\n");  }
					case .Difference; 		  { print( "(-)\n");  }
					case .Exterior_Product;   { print( "(^)\n");  }
					case .Regressive_Product; { print( "(!^)\n"); }
				}
				print_ast_helper( binary_operator.left,  indent + 2 );
				print_ast_helper( binary_operator.right, indent + 2 );
			}
			case .Unary_Op;  { 
				unary_operator := cast(*Unary_Operator_Node)node;
				print( "Unary Operator" );
				if unary_operator.op == {
					case .Left_Complement;  { print( "(<<)\n" ); }
					case .Right_Complement; { print( "(>>)\n" ); }
					case .Negation;         { print( "(-) \n" ); }
				}
				print_ast_helper( unary_operator.operand, indent + 2 );
			}
			case; { print( "Unrecognized\n" ); }
		}
	}

	print_ast_helper( ast );
}

#scope_file

peek_next_token :: inline ( parser: *Parser ) -> ( token: *Token ) {
	return *parser.tokens[parser.token_cursor];
}

consume_token :: inline ( parser: *Parser ) {
	parser.token_cursor += 1;
	return;
}

// keywords need to be in here!! i think!!!
// need to rethink the use of t0,t1, and t2, might be confusing later on
parse_statement :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	t0: *Token;
	t1: *Token;
	t2: *Token;
	success: bool;

	node: *Ast_Node = null;

	t0 = peek_next_token( parser );
	if t0.type == .Identifier {
		consume_token( parser );

		t1 = peek_next_token( parser );
		if t1.type == .Assign {
			consume_token( parser );

			left  := new_identifier_node( t0.identifier );
			right := parse_expression( parser );

			t2 = peek_next_token( parser );
			if t2.type == .Semicolon {
				consume_token( parser );
				binary_op := new_binary_operator_node( .Assign, left, right );
				node = binary_op;
			}
			else {
				print( "Parse error, expected a semicolon following the assignment statement\n" );
			}
		}
		else
		if t1.type == .Semicolon {
			
			ident := new_identifier_node( t0.identifier );
			node = ident;
		}
		else {
			print( "Parse error, expected an equals sign or semicolon following identifier\n" );
		}
	}
	else {
		print( "Parse error, identifier expected at the start of each statement\n" );
	}

	return node;
}

parse_expression :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	term: *Ast_Node = null;
	term = parse_exterior_term( parser );
	
	while true {	
			
		t0 := peek_next_token( parser );
		if t0.type == .Semicolon || t0.type == .End_Of_Input {
			break;
		}
		
		if t0.type == .Plus || t0.type == .Minus {
			op: Binary_Op;
			if t0.type == {
				case .Plus;  { op = Binary_Op.Sum; }
				case .Minus; { op = Binary_Op.Difference; }
			}
			consume_token( parser );

			next_term: *Ast_Node;
			next_term = parse_exterior_term( parser );

			// Since sums and differences can't easily be resolved in Grassmann Algebra
			// and often times lead to multi-vectors I just want to deal strictly with sums.
			// Especially a pain in the ass when you have the same k-element at different levels a
			// of the tree and you want to collect terms. 
			//
			// Converting differences to sums doesn't solve the 'collecting like terms' problem but
			// it is one set of if statements I won't have to worry about later.
			//      + 					+
			//   e1  -         ==>   e1    +
			//     e2 +                  e1 +
			//       e1 e3                -e1 -e3
			//

			if op == Binary_Op.Difference {
				parent_node := new_unary_operator_node( Unary_Op.Negation, next_term );
				next_term = parent_node;

				op = Binary_Op.Sum;
			}

			node := new_binary_operator_node( op, term, next_term );

			term = node;
		}
		else {
			break;
		}
	}

	return term;
}

parse_exterior_term :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	factor: *Ast_Node = null;
	factor = parse_exterior_factor( parser );

	while true {
		t0 := peek_next_token( parser );

		if t0.type == .Semicolon || t0.type == .End_Of_Input {
			break;
		}

		if t0.type == .Exterior_Product || t0.type == .Regressive_Product {
			consume_token( parser );
			op: Binary_Op;
			if t0.type == {
				case .Exterior_Product;   { op = Binary_Op.Exterior_Product;   }
				case .Regressive_Product; { op = Binary_Op.Regressive_Product; }
			}

			next_factor: *Ast_Node;
			next_factor = parse_exterior_factor( parser );

			node := new_binary_operator_node( op, factor, next_factor );

			factor = node;
		}
		else {
			break;
		}
	}

	return factor;
}

parse_exterior_factor :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	node: *Ast_Node = null;

	t0: *Token;
	t1: *Token;

	// capture unary operators and apply after parsing the factor
	parser.temp_cursor = 0;
	while true {
		t0 = peek_next_token( parser );
		if t0.type == {
			case .Minus; {
				consume_token( parser );
				parser.temp_buffer[parser.temp_cursor] = Unary_Op.Negation;
				parser.temp_cursor += 1;
			}
			case .Left_Complement; {
				consume_token( parser );
				parser.temp_buffer[parser.temp_cursor] = Unary_Op.Left_Complement;
				parser.temp_cursor += 1;
			}
			case .Right_Complement; {
				consume_token( parser );
				parser.temp_buffer[parser.temp_cursor] = Unary_Op.Right_Complement;
				parser.temp_cursor += 1;
			}
			case; { break; }
		}
	}

	t0 = peek_next_token( parser );
	if t0.type == .Basis_Element {
		consume_token( parser );
		k_element := new_k_element_node( grade = 1, coefficient = 1, one_element_components = t0.basis_element_index );
		node = k_element;
	}
	else
	if t0.type == .Identifier {
		consume_token( parser );
		ident := new_identifier_node( t0.identifier );
		node  = ident;
	}
	else
	if t0.type == .Number {
		consume_token( parser );
		literal := new_literal_node( t0.number );
		node = literal;

		t1 = peek_next_token( parser );
		if t1.type == .Identifier {
			consume_token( parser );
			identifier := new_identifier_node( t1.identifier );
			binary_op := new_binary_operator_node( .Exterior_Product, literal, identifier );

			node = binary_op;	
		}
		else
		if t1.type == .Basis_Element {
			consume_token( parser );
			k_element := new_k_element_node( grade = 1, coefficient = 1, one_element_components = t1.basis_element_index );
			binary_op := new_binary_operator_node( .Exterior_Product, literal, k_element );

			node = binary_op;			
		}
	}
	else
	if t0.type == .Open_Paren {
		consume_token( parser );
		node = parse_expression( parser );
		t0 = peek_next_token( parser );
		if t0.type  == .Close_Paren {
			consume_token( parser );
		}
		else {
			// report error, missing close paren on expression!
		}
	}
	else {
		// report error, unable to parse expression!
	}

	for i: 0..parser.temp_cursor-1 {
		parent_node := new_unary_operator_node( parser.temp_buffer[i], node );
		node = parent_node;
	}

	return node;
}
