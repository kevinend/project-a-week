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
	
	error_reported: bool = false; // maybe remove this and try the poisoning???
	error_message:  string;
	
	tokens: 	  [] Token;
	token_cursor: int;

	PARSER_TEMP_BUFFER_LENGTH :: 8;

	temp_buffer: [PARSER_TEMP_BUFFER_LENGTH] Unary_Op;

	ast: *Ast_Node;
}

// TODO: remove the symbol table, that would be handled in the semantic phase of the operation
//       typecheck
//       do a semantic check, report errors
//       do the identifier lookup
//       do the expansion
//       do the evaluation
// seems like a lot of steps
// what if the tokens were prefix like LISP?
//  (+ 3 4)
//  but if you have something like e1^e2 + e3
//  
// + ^ e1 e2, that would be harder to do since we don't know how many tokens to look ahead
// and you'd have to do that repeatedly
// 
// should return a parser WITH errors not a parser or errors, which is kind of what i am doing
// Command_Set || Command_Get, parse the command
// 
// parse assignments
// parse expressions
// parse commands -- these are the top level things we could have in the tree
// 
// when you parse the expression you start with the terms in the expression (assuming simple sums)
//  category.Expression (number, identifier, or basis element), maybe it isn't a good idea
//  category.Literal    (literal)
//  category.Operator   (
//  category.Separator  (
//  category.Command    ( 
//  category.Error      ( all unrecognized symbols )
//  category.Text
//  category.Number
// 
// then you are an error and you poison this. Syntax error, unable to parse expression to the right of blah,
// expected number, identifier or basis element and found token '$whatever'
// feel like that would be relatively trivial pass over the expression and still be relatively quick
//
// so lets take that case
// e1 +; // missing an argument
// parse expression
//  parse +
//   grab the next token, if it's category is not an expression, then we return the error token
// or we could just put the error in there for now, i feel like that is
// we could say if you are a Unary_Operator or Expression
// Binary_Operator
// Unary_Operator
// category.Expression or a Unary_Operator, expected a unary operator or an expression
// 
//  Categories:
//    Expression
//    Unary_Operator
//    Binary_Operator
//    Separator
//    Command
//    Error
//
// but i kind of feel that it is
// parse_assignment
// parse_get_command
// parse_set_command
// parse_exit_command
// i feel like these all need different procedures
// parse_expression
//


generate_ast :: ( parser: *Parser, tokens: [] Token, num_tokens: int, symbol_table: *Symbol_Table ) {

	reset_parser( parser, tokens, num_tokens );

	root: *Ast_Node = null;
	t0: Token;	
	t0 = peek_token( parser );
	if t0.type > .Command_Start && t0.type < .Command_End {
		root = parse_command( parser );
	}
	else {
		root = parse_statement( parser, symbol_table ); 
	}

	parser.ast = root;

}

print_ast :: ( parser: *Parser, header: string ) {

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

	print_ast_helper( parser.ast );
	print( "\n" );
}

#scope_file

reset_parser :: ( parser: *Parser, tokens: [] Token, num_tokens: int ) {

	parser.tokens 		= tokens;
	parser.tokens.count = num_tokens;

	parser.token_cursor = 0;
	parser.ast          = null;

	parser.error_reported = false;

	return;
}

peek_token :: inline ( parser: *Parser, offset: int = 0 ) -> ( token: *Token ) {
	return *parser.tokens[parser.token_cursor + offset];
}

consume_token :: inline ( parser: *Parser ) {
	parser.token_cursor += 1;
}

report_error :: ( parser: *Parser, message: string ) {

	// Parser errors:
	// Error messages are written to temporary storage, will be wiped on each iteration of the program.
	token := parser.tokens[parser.token_cursor];
	default_message := tprint( "Parser error occurred on line number %, character %\n", token.line_number, token.character_index );

	error_message: string;
	error_message.count = default_message.count + message.count;
	error_message.data  = talloc( error_message.count );
	assert( error_message.data != null );

	memcpy( error_message.data, default_message.data, default_message.count );
	memcpy( error_message.data + default_message.count, message.data, message.count );

	parser.error_reported = true;
	parser.error_message  = error_message;
}

parse_command :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	t0: Token;

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
	
		t0 = peek_token( parser );
		if t0.type < .Command_Start || t0.type > .Command_End {
			report_error( parser, "Unrecognized command.\n" );
			break command_loop;
		}
		command = t0.type;
		consume_token( parser );

		if command == .Keyword_Exit {
			// Hack! This is the only command thus far that doesn't have a config or args following it
			node = new_command_node( Command.Exit, 0, args );
			break command_loop;
		}

		t0 = peek_token( parser );
		if t0.type < .Config_Start || t0.type > .Config_End {
			report_error( parser, "Unrecognized configuration following command.\n" );
			break command_loop;
		}
		config = t0.type;
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
			t0 = peek_token( parser );
			if t0.type != .Assign {
				report_error( parser, "Syntax error, expected the assignment operator following the keyword set\n" );
				break command_loop;
			}
			consume_token( parser );

			if config == .Keyword_Dimension {
				t0 = peek_token( parser );
				if t0.type != .Literal {
					report_error( parser, "Syntax error, expected a literal number following the command 'set dimension'\n" );
					break command_loop;
				}
				array_add( *args, new_literal_node( t0.text ) );  // janky, should be able to pass unquoted number in statement? hmm
				consume_token( parser );

				node = new_command_node( Command.Set, Config.Dimension, args );
			}
			else
			if config == .Keyword_Basis {
				t0 = peek_token( parser );
				if t0.type != .Open_Square_Brace { 
					report_error( parser, "Syntax error, expected opening brace [ before command arguments\n" );
					break command_loop; 
				}
				consume_token( parser );
			
				t0 = peek_token( parser );
				while true {
					if t0.type != .Literal { 
						report_error( parser, "Syntax error, commands arguments must be a literal value, example 'e1'\n" );
						break command_loop;
					}
					
					literal := new_literal_node( t0.text );
					array_add( *args, literal );
					consume_token( parser );

					
					t0 = peek_token( parser );
					if t0.type == .Close_Square_Brace || t0.type == .End_Of_Input {
						// the token will be a closing square brace on the last literal in the list of arguments
						break;
					}
					else if t0.type != .Comma {
						report_error( parser, "Syntax error, arguments must be separated by a comma, example ['e1','e2',...]\n" );
						break command_loop;
					}

					consume_token( parser );

					t0 = peek_token( parser );
				}
				
				if t0.type == .End_Of_Input { 
					report_error( parser, "Syntax error, malformed command.\n" );
					break command_loop;
				}

				// found a closing square brace instead of the end of input token
				consume_token( parser );

				node = new_command_node( Command.Set, Config.Basis, args );
			}
		}

		break command_loop;
	}

	t0 = peek_token( parser );
	if !parser.error_reported && t0.type != .Semicolon {
		report_error( parser, "Syntax error, all commands must end with a semicolon\n" );
		node = null;
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

	statement: *Ast_Node = null;

	t0 = peek_token( parser );
	t1 = peek_token( parser, offset = 1 );

	if t1.type == .Assign {
		if t0.type != .Identifier {
			report_error( parser, "Syntax error, only identifiers can be specified on the left of an assignment operator\n" );
			return null;
		}

		consume_token( parser ); // identifier
		consume_token( parser ); // assignment

		ident := new_identifier_node( t0.identifier );
		expr  := parse_expression( parser, symbol_table );

		assignment := new_binary_operator_node( .Assign, ident, expr );
		statement = assignment;
	}
	else {
		statement = parse_expression( parser, symbol_table );	
	}

	t0 = peek_token( parser );
	if !parser.error_reported {
		// these are uncaught errors that need to be handled
		if t0.type == .Minus || t0.type == .Left_Complement || t0.type == .Right_Complement {
			report_error( parser, "Syntax error, no expression found to the right of the unary operator\n" );
			return null;
		}
		else 
		if t0.type != .Semicolon {
			report_error( parser, "Syntax error, all statements must end with a semicolon\n" );
			return null;
		}
	}
	consume_token( parser );

	return statement;
}

parse_expression :: ( parser: *Parser, symbol_table: *Symbol_Table ) -> ( node: *Ast_Node ) {

	term: *Ast_Node = null;
	term = parse_exterior_term( parser, symbol_table );
	
	while true {	
			
		t0 := peek_token( parser );
		if t0.type == .Semicolon || t0.type == .End_Of_Input { break; }

		if t0.type == .Plus || t0.type == .Minus {
			op: Binary_Op;
			if t0.type == {
				case .Plus;  { op = Binary_Op.Sum; }
				case .Minus; { op = Binary_Op.Difference; }
			}
			consume_token( parser );

			next_term: *Ast_Node;
			next_term = parse_exterior_term( parser, symbol_table );

			if next_term == null {
				report_error( parser, tprint( "Syntax error, unable to parse expression to the right of the binary operator %\n", op ) );
				return null;
			}

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

		if t0.type == .Semicolon || t0.type == .End_Of_Input { break; }

		if t0.type == .Exterior_Product || t0.type == .Regressive_Product {
			consume_token( parser );
			op: Binary_Op;
			if t0.type == {
				case .Exterior_Product;   { op = Binary_Op.Exterior_Product;   }
				case .Regressive_Product; { op = Binary_Op.Regressive_Product; }
			}

			next_factor: *Ast_Node;
			next_factor = parse_exterior_factor( parser, symbol_table );

			if next_factor == null {
				report_error( parser, tprint( "Syntax error, unable to parse expression to the right of binary operator %\n", op ) );
				return null;
			}

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
	t0:   *Token;

	// capture unary operators and apply after parsing the factor.
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

	if num_unary_operators > 0   &&
	   t0.type != .Basis_Element && 
	   t0.type != .Identifier    && 
	   t0.type != .Number        && 
	   t0.type != .Open_Paren {
		report_error( parser, "Syntax error, no expression following unary operator\n" );
		return null;
	}

	if t0.type == .Basis_Element {
		consume_token( parser );
		k_element := new_k_element_node( coefficient = 1, basis = t0.basis );

/*
		t0 = peek_token( parser );
		if token isn't an operator		
		return null
*/
		
		node = k_element;
	}
	else
	if t0.type == .Identifier {
		consume_token( parser );
		value, success := lookup_symbol( t0.identifier, symbol_table );
		if !success {
			report_error( parser, tprint( "Syntax error, unresolved identifier %\n", t0.identifier ) );
			return null;
		}
		ident := multielement_to_ast( value );

/*
		t0 = peek_token( parser );
		if token isn't an operator (unary operator, binary_operator)
		return null
*/

		node = ident;
	}
	else
	if t0.type == .Number {
		consume_token( parser );
		scalar := new_k_element_node( coefficient = t0.number, basis = 0 );

		t0 = peek_token( parser );
		if t0.type == .Identifier {
			consume_token( parser );
			value, success := lookup_symbol( t0.identifier, symbol_table );
			if !success {
				report_error( parser, tprint( "Syntax error, unresolved identifier %\n", t0.identifier ) );
				return null;
			}

			exterior_product := new_binary_operator_node( .Exterior_Product, scalar, multielement_to_ast( value ) );
			node = exterior_product;
		}
		else
		if t0.type == .Basis_Element {
			consume_token( parser );
			k_element := new_k_element_node( coefficient = 1, basis = t0.basis );
			exterior_product := new_binary_operator_node( .Exterior_Product, scalar, k_element );

			node = exterior_product;			
		}
		else
		if t0.type == .Open_Paren {
			consume_token( parser );
			expression := parse_expression( parser, symbol_table );

			t0 = peek_token( parser );
			if t0.type != .Close_Paren {
				report_error( parser, "Syntax error, msising closing parenthesis in expression\n" );
				return null;
			}
			consume_token( parser );
			
			exterior_product := new_binary_operator_node( .Exterior_Product, scalar, expression );
			node = exterior_product;
		}
		else {
			node = scalar;
		}
	}
	else
	if t0.type == .Open_Paren {
		consume_token( parser );

		expression := parse_expression( parser, symbol_table );

		t0 = peek_token( parser );
		if t0.type  != .Close_Paren {
			report_error( parser, "Syntax error, missing closing parenthesis in expression\n" );
			return null;
		}
		consume_token( parser );

		node = expression;
	}
	else {
		// errors reported in the calling procedures
		return null;
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
