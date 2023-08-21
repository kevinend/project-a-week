#scope_export

// TODO: Need 'Complement_Variants' in eval file
// TODO: can we do the #load in a different order?
//  Eval needs the parser
//  The parser needs the lexer
//  main needs the lexer, can we import in that order???
// TODO: Update the BNF on the header
// TODO: Add the keywords in here

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

	tokens: [] Token;
	token_cursor: int;

	temp_buffer: [PARSER_TEMP_BUFFER_LENGTH] Unary_Operation;
	temp_cursor: int;

	ast: *Ast_Node;

	error: string; // not capturing this at the moment!
}


Node_Kind :: enum {
	
	K_Element; // support for 'origin' as basis, so no longer just k-vectors
	Unary_Operator;
	Binary_Operator;
	Identifier;
	Literal;
}

Ast_Node :: struct {
	kind: Node_Kind;
}

K_Element_Node :: struct {

	#as using _kind: Ast_Node;
	
	grade: int;
	coefficient: int; 	 		 // lexer returns integers
	one_element_components: s64; // bit field, supports up to 64 basis one elements
}

Unary_Operation :: enum {
	
	Negation;
	Left_\Complement;
	Right_Complement;
}

Unary_Operator_Node :: struct {
	
	#as using _kind: Ast_Node;

	operation: Unary_Operation;
	operand: *Ast_Node;
}

Binary_Operation :: enum {
	
	Assign;
	Sum;
	Difference;
	Exterior_Product;
	Regressive_Product;
	Interior_Product;
}

Binary_Operator_Node :: struct {

	#as using _kind: Ast_Node;

	operation: Binary_Operation;
	left_\operand: *Ast_Node;
	right_operand: *Ast_Node;
}

Identifier_Node :: struct {

	#as using _kind: Ast_Node;
	name: string;
}

Literal_Node :: struct {
	#as using _kind: Ast_Node;
	value: int;
}

update_parser :: ( parser: *Parser, tokens: [] Token, num_tokens: int ) {
	
	parser.tokens 		= tokens;
	parser.tokens.count = num_tokens;

	parser.token_cursor = 0;
	parser.ast          = null;

	return;
}

generate_abstract_expression_tree :: ( parser: *Parser ) {

	// every valid user input starts with an identifier

	root: *Ast_Node;
	root = parse_statement( parser );

	parser.ast = root;
}

// need to fill this out more but it looks better!
print_ast :: ( ast: *Ast_Node ) {
	
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
				print( "K-Element\n" ); 
			}
			case .Identifier; { 
				print( "Identifier\n" ); 
			}
			case .Binary_Operator; { 
				print( "Binary Operator\n" ); 

				binary_operator := cast(*Binary_Operator_Node)node;
				print_ast_helper( binary_operator.left\_operand, indent + 2 );
				print_ast_helper( binary_operator.right_operand, indent + 2 );
			}
			case .Unary_Operator;  { 
				print( "Unary Operator\n"  );

				unary_operator := cast(*Unary_Operator_Node)node;
				print_ast_helper( unary_operator, indent + 2 );
			}
			case; { print( "Unrecognized\n" ); }
		}
	}

	print_ast_helper( ast );
}


#scope_file


init_k_element_node :: ( node: *K_Element_Node, grade: int, coefficient: int, one_element_components: int ) {

	node.kind  		 			= Node_Kind.K_Element;
	node.grade 		 			= grade;
	node.coefficient 		    = coefficient;
	node.one_element_components = one_element_components;

	return;
}

init_unary_operator_node :: ( node: *Unary_Operator_Node, operation: Unary_Operation, operand: *Ast_Node ) {

	node.kind 	   = Node_Kind.Unary_Operator;
	node.operation = operation;
	node.operand   = operand;

	return;
}

init_binary_operator_node :: ( node: *Binary_Operator_Node, operation: Binary_Operation, left: *Ast_Node, right: *Ast_Node ) {

	node.kind 	   	   = Node_Kind.Binary_Operator;
	node.operation 	   = operation;
	node.left\_operand = left;
	node.right_operand = right;

	return;
}

init_identifier_node :: ( node: *Identifier_Node, name: string ) {
	
	node.kind = Node_Kind.Identifier;
	node.name = copy_string( name ); // seems wasteful, can we intern???

	return;
}

init_literal_node :: ( node: *Literal_Node, value: int ) {
	
	node.kind = Node_Kind.Literal;
	node.value = value;

	return;
}

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

			left := New(Identifier_Node);
			init_identifier_node( left, t0.identifier );

			right := parse_expression( parser );

			t2 = peek_next_token( parser );
			if t2.type == .Semicolon {
				consume_token( parser );
				binary_op := New(Binary_Operator_Node);
				init_binary_operator_node( binary_op, .Assign, left, right );

				node = binary_op;
			}
			else {
				print( "Parse error, expected a semicolon following the assignment statement\n" );
			}
		}
		else
		if t1.type == .Semicolon {
			
			ident := New(Identifier_Node);
			init_identifier_node( ident, t0.identifier );
				
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
			operation: Binary_Operation;
			if t0.type == {
				case .Plus;  { operation = Binary_Operation.Sum; }
				case .Minus; { operation = Binary_Operation.Difference; }
			}
			consume_token( parser );

			next_term: *Ast_Node;
			next_term = parse_exterior_term( parser );

			node := New(Binary_Operator_Node);
			init_binary_operator_node( node, operation, term, next_term );

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
			operation: Binary_Operation;
			if t0.type == {
				case .Exterior_Product;   { operation = Binary_Operation.Exterior_Product;   }
				case .Regressive_Product; { operation = Binary_Operation.Regressive_Product; }
			}

			next_factor: *Ast_Node;
			next_factor = parse_exterior_factor( parser );

			node := New(Binary_Operator_Node);
			init_binary_operator_node( node, operation, factor, next_factor );

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
				parser.temp_buffer[parser.temp_cursor] = Unary_Operation.Negation;
				parser.temp_cursor += 1;
			}
			case .Left_Complement; {
				consume_token( parser );
				parser.temp_buffer[parser.temp_cursor] = Unary_Operation.Left_Complement;
				parser.temp_cursor += 1;
			}
			case .Right_Complement; {
				consume_token( parser );
				parser.temp_buffer[parser.temp_cursor] = Unary_Operation.Right_Complement;
				parser.temp_cursor += 1;
			}
			case; { break; }
		}
	}

	t0 = peek_next_token( parser );
	if t0.type == .Basis_Element {
		consume_token( parser );

		k_element := New(K_Element_Node);
		init_k_element_node( k_element, grade = 1, coefficient = 1, one_element_components = t0.basis_element_index );

		node = k_element;
	}
	else
	if t0.type == .Identifier {
		consume_token( parser );

		ident := New(Identifier_Node);
		init_identifier_node( ident, t0.identifier );

		node = ident;
	}
	else
	if t0.type == .Number {
		consume_token( parser );
		literal := New(Literal_Node);
		init_literal_node( literal, t0.number );

		node = literal;

		t1 = peek_next_token( parser );
		if t1.type == .Identifier {
			consume_token( parser );
			identifier := New(Identifier_Node);
			init_identifier_node( identifier, t1.identifier );

			binary_op := New(Binary_Operator_Node);
			init_binary_operator_node(
				 binary_op
				,.Exterior_Product
				,literal
				,identifier
			);

			node = binary_op;	
		}
		else
		if t1.type == .Basis_Element {
			consume_token( parser );
			k_element := New(K_Element_Node);
			init_k_element_node( k_element, grade = 1, coefficient = 1, one_element_components = t1.basis_element_index );

			binary_op := New(Binary_Operator_Node);
			init_binary_operator_node(
				 binary_op
				,.Exterior_Product
				,literal
				,k_element
			);

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
		parent_node := New(Unary_Operator_Node);
		init_unary_operator_node( parent_node, parser.temp_buffer[i], node );

		node = parent_node;
	}

	return node;
}

#import "Basic";
