#scope_export

/*
	BNF for Grassmann expressions:
		statement  		::= <identifier> "=" <expression> ";"
		statement       ::= <identifier> ";"

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"].. ["<<"].. [">>"].. ( <basis-one-element> | <number> | <identifier> | "(" <expression> ")" )		
*/

Parser :: struct {

	PARSER_TEMP_BUFFER_LENGTH :: 8;

	tokens: 	  [] Token;
	token_cursor: int;

	temp_buffer: [PARSER_TEMP_BUFFER_LENGTH] Unary_Op;

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

parser_generate_ast :: ( parser: *Parser, symbol_table: *Symbol_Table ) {

	t: Token;
	root: *Ast_Node = null;
	
	t = peek_token( parser );
	if t.type > .Command_Start && t.type < .Command_End {
		root = parse_command( parser );
	}
	else {
		root = parse_statement( parser, symbol_table ); 
	}

	parser.ast = root;
}

print_ast :: ( ast: *Ast_Node, header: string ) {

	print( "%\n", header );
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
				print( "K-Element(Coefficient=%, Basis=%)\n", k_element.coefficient, k_element.basis ); 
			}
			case .Literal; {
				print( "Literal\n" );
			}
			case .Identifier; { 
				ident := cast(*Identifier_Node)node;
				print( "Identifier(Name=%)\n", ident.name ); 
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
			case .Command; {
				print( "Found a command!\n" );
			}
			
			case; { print( "Unrecognized\n" ); }
		}
	}

	print_ast_helper( ast );
	print( "\n" );
}

#scope_file

peek_token :: inline ( parser: *Parser, offset: int = 0 ) -> ( token: *Token ) {
	return *parser.tokens[parser.token_cursor + offset];
}

consume_token :: inline ( parser: *Parser ) {
	parser.token_cursor += 1;
	return;
}

// Do i want separate GET and SET nodes and EXIT node, or just a single COMMAND node?

parse_command :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	t: Token;

	command: Token_Type;
	config:  Token_Type;
	args:    [..] *Ast_Node;
	args.allocator = temp;

	node: *Ast_Node = null;

	// Jai does not support 'Goto'.
	// Without Goto the code becomes incredibly verbose and nested.
	// Fudging the goto with a loop that iterates a single time and breaks at the end.
	// If there is an error during parsing we report the error in the parser then break out of the loop
	// effectively mirroring Goto's functionality.

	while command_loop := true {
	
		t = peek_token( parser );
		if t.type < .Command_Start || t.type > .Command_End {
			// report parse error
			break command_loop;
		}
		command = t.type;
		consume_token( parser );

		if command == .Keyword_Exit {
			// hacky, this is the only command thus far that doesn't have a config or args following it
			node = new_command_node( Command.Exit, 0, args );
			break command_loop;
		}

		t = peek_token( parser );
		if t.type < .Config_Start || t.type > .Config_End {
			// report parse error
			break command_loop;
		}
		config = t.type;
		consume_token( parser );

		if command == .Keyword_Get {
			// supported get commands don't have arguments so the form is 'get {config};'. Create command node for the command/config combination.
			if config == .Keyword_Dimension {
				node = new_command_node( Command.Get, Config.Dimension, args );	
			}
			else
			if config == .Keyword_Basis {
				node = new_command_node( Command.Get, Config.Basis, args );
			}
		}
		else
		if command == .Keyword_Set {
			// all set commands require arguments so they are of the form 'set {config} = {args};'
			t = peek_token( parser );
			if t.type != .Assign {
				// report parse error
				break command_loop;
			}
			consume_token( parser );

			if config == .Keyword_Dimension {
				t = peek_token( parser );
				if t.type != .Literal {
					// report parse error
					break command_loop;
				}
				array_add( *args, new_literal_node( t.text ) );  // janky, should be able to pass unquoted number in statement? hmm
				consume_token( parser );

				node = new_command_node( Command.Set, Config.Dimension, args );
			}
			else
			if config == .Keyword_Basis {
				t = peek_token( parser );
				if t.type != .Open_Square_Brace { 
					// report parse error 
					break command_loop; 
				}
				consume_token( parser );
			
				t = peek_token( parser );
				while true {
					if t.type != .Literal { 
						// report parse error; 
						break command_loop;
					}
					
					literal := new_literal_node( t.text );
					array_add( *args, literal );
					consume_token( parser );

					
					t = peek_token( parser );
					if t.type == .Close_Square_Brace || t.type == .End_Of_Input {
						// the token will be a closing square brace on the last literal in the list of arguments
						break;
					}
					else if t.type != .Comma {
						// report parse error
						break command_loop;
					}

					consume_token( parser );

					t = peek_token( parser );
				}
				
				if t.type == .End_Of_Input { 
					// report parse error; 
					break command_loop;
				}

				// found a closing square brace instead of the end of input token
				consume_token( parser );

				node = new_command_node( Command.Set, Config.Basis, args );
			}
		}

		break command_loop;
	}

	// all commands must end with a semicolon to be valid!
	t = peek_token( parser );
	if t.type != .Semicolon {
		// report parse error
	}
	consume_token( parser );

	return node;	
}

parse_statement :: ( parser: *Parser, symbol_table: *Symbol_Table ) -> ( node: *Ast_Node ) {

	// Valid Statements:
	//  <statement> ::= <ident> '=' <expression> ';'
	//  <statement> ::= <expression>;
	//
	// To determine if the statement is an assignment we need to read two tokens.
	// Hacky but if there is only a single token in the expression we append the END_OF_INPUT
	// token onto the end so there will always be two tokens in the buffer.
	// If the input is empty the main loop just continues to the next iteration as well.

	t0: *Token;
	t1: *Token;
	t2: *Token;
	success: bool;

	node: *Ast_Node = null;

	t0 = peek_token( parser );
	t1 = peek_token( parser, offset = 1 );

	if t0.type == .Identifier && t1.type == .Assign {
		// assignment statement
		consume_token( parser );
		consume_token( parser );

		left  := new_identifier_node( t0.identifier );
		right := parse_expression( parser, symbol_table );

		t2 = peek_token( parser );
		if t2.type == .Semicolon {
			consume_token( parser );
			binary_op := new_binary_operator_node( .Assign, left, right );
			node = binary_op;
		}
		else {
			print( "Parse error, expected a semicolon following an assignment statement\n" );
		}
	}
	else {
		node = parse_expression( parser, symbol_table );
	}
	
	return node;
}

parse_expression :: ( parser: *Parser, symbol_table: *Symbol_Table ) -> ( node: *Ast_Node ) {

	term: *Ast_Node = null;
	term = parse_exterior_term( parser, symbol_table );
	
	while true {	
			
		t0 := peek_token( parser );
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
			next_term = parse_exterior_term( parser, symbol_table );

			// Since sums and differences can't easily be resolved in Grassmann Algebra
			// and often times lead to multi-vectors/multi-elements I just want to deal strictly with sums.
			// Especially a pain in the ass when you have the same k-element at different levels a
			// of the tree and you want to collect terms. 
			//
			// Converting differences to sums doesn't solve the 'collecting like terms' problem but
			// it is one less set of if statements I won't have to worry about later.
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

parse_exterior_term :: ( parser: *Parser, symbol_table: *Symbol_Table ) -> ( node: *Ast_Node ) {

	factor: *Ast_Node = null;
	factor = parse_exterior_factor( parser, symbol_table );

	while true {
		t0 := peek_token( parser );

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
			next_factor = parse_exterior_factor( parser, symbol_table );

			node := new_binary_operator_node( op, factor, next_factor );

			factor = node;
		}
		else {
			break;
		}
	}

	return factor;
}

parse_exterior_factor :: ( parser: *Parser, symbol_table: *Symbol_Table ) -> ( node: *Ast_Node ) {

	node: *Ast_Node = null;

	t0: *Token;
	t1: *Token;

	// capture unary operators and apply after parsing the factor
	num_unary_operators: int = 0;
	while true {
		t0 = peek_token( parser );
		if t0.type == {
			case .Minus; {
				consume_token( parser );
				parser.temp_buffer[num_unary_operators] = Unary_Op.Negation;
				num_unary_operators += 1;
			}
			case .Left_Complement; {
				consume_token( parser );
				parser.temp_buffer[num_unary_operators] = Unary_Op.Left_Complement;
				num_unary_operators += 1;
			}
			case .Right_Complement; {
				consume_token( parser );
				parser.temp_buffer[num_unary_operators] = Unary_Op.Right_Complement;
				num_unary_operators += 1;
			}
			case; { break; }
		}
	}

	t0 = peek_token( parser );
	if t0.type == .Basis_Element {
		consume_token( parser );
		k_element := new_k_element_node( coefficient = 1, basis = t0.basis );
		node = k_element;
	}
	else
	if t0.type == .Identifier {
		consume_token( parser );
		value, success := lookup_symbol( t0.identifier, symbol_table );
		if !success {
			print( "Unresolved identifier %\n", t0.identifier );
		}
		node = multielement_to_ast( value );
	}
	else
	if t0.type == .Number {
		consume_token( parser );
		scalar := new_k_element_node( coefficient = t0.number, basis = 0 );
		node = scalar;

		t1 = peek_token( parser );
		if t1.type == .Identifier {
			consume_token( parser );
			value, success := lookup_symbol( t1.identifier, symbol_table );
			if !success {
				print( "Unresolved identifier %\n", t1.identifier );
			}
			binary_op := new_binary_operator_node( .Exterior_Product, scalar, multielement_to_ast( value ) );

			node = binary_op;
		}
		else
		if t1.type == .Basis_Element {
			consume_token( parser );
			k_element := new_k_element_node( coefficient = 1, basis = t1.basis );
			binary_op := new_binary_operator_node( .Exterior_Product, scalar, k_element );

			node = binary_op;			
		}
	}
	else
	if t0.type == .Open_Paren {
		consume_token( parser );
		node = parse_expression( parser, symbol_table );
		t0 = peek_token( parser );
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

	// iterate backwards over the set of unary operators
	// creating a new parent that points to the later unary operator in lexicographical order
	i: int = num_unary_operators-1;	
	while i >= 0 {
		defer i -= 1;
		parent_node := new_unary_operator_node( parser.temp_buffer[i], node );
		node = parent_node;
	}

	return node;
}
