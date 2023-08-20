#scope_export

// TODO: Need 'Complement_Variants' in eval file
// TODO: can we do the #load in a different order?
//  Eval needs the parser
//  The parser needs the lexer
//  main needs the lexer, can we import in that order???

/*
	BNF for Grassmann expressions: (ORIGINAL)

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"] ["LC","RC"] ( <basis-one-element> | <number> |  | "(" <expression> ")" )

	THIS IS INACCURATE: SUE ME!!
	BNF for Grassmann expressions: (REVISED)

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"] ["LC","RC"] ( <basis-one-element> | <number> |  | "(" <expression> ")" )


		statement  		::= <identifier> "=" <expression> // this can't be recursive, this allows for shit like x = y = z = t;
		expression 		::= <identifier> ";"
		expression 		::= <exterior-term> [("+","-") <exterior-term]...
		exterior-term   ::=
		
*/

Parser :: struct {

	tokens: [] Token;
	token_cursor: int;

	ast: *Ast_Node;
}


Node_Kind :: enum {
	
	K_Element; // support for 'origin' as basis, so no longer just k-vectors
	Unary_Operator;
	Binary_Operator;
	Identifier;
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
	name: string
}

update_parser :: ( parser: *Parser, tokens: [] Token ) {
	
	parser.tokens 		= tokens;
	parser.token_cursor = 0;
	parser.ast          = null;

	return;
}

generate_abstract_expression_tree :: ( tokens: [] Token ) -> ( root: *Ast_Node ) {

	// every valid user input starts with an identifier
	// parse_expression( takes the current token i guess)

	root: *Ast_Node;
	root = parse_statement( tokens );

	return root;
}


#scope_file

init_binary_operator :: ( node: *Binary_Operator_Node, operation: Binary_Operation, left: *Ast_Node, right: *Ast_Node ) {

	node.kind 	   	   = Node_Kind.Binary_Operator;
	node.operation 	   = operation;
	node.left\_operand = left;
	node.right_operand = right;
}

init_identifier_node :: ( node: *Identifier_Node, name: string ) {
	
	node.kind = Node_Kind.Identifier;
	node.name = copy_string( name ); // seems wasteful, can we intern???

	return;
}

contains_more_tokens :: inline ( parser: *Parser ) -> ( yes_no: bool ) {
	return parser.tokens.count - parser.token_cursor;
}

peek_next_token :: inline ( tokens: [] Token ) -> ( success: bool, token: *Token ) {

	if !contains_more_tokens( parser ) {
		return false, null;
	}
	else {
		return true, *parser.tokens[parser.token_cursor];
	}
}

consume_token :: inline ( parser: *Parser ) {
	parser.token_cursor += 1;
}

// you are not going to get this right the first time kev
// this is a big undertaking and you need to fumble around, maybe for a couple of days
// to get the lay of the land
parse_statement :: ( parser: *Parser ) -> ( node: *Ast_Node ) {

	t0: *Token;
	t1: *Token;
	success: bool;

	node: *Ast_Node; // needs set on return!!!

	success, t0 := peek_next_token( parser );
	if success && t0.type == .Identifier {

		consume_token( parser );

		success, t1 = peek_next_token( parser );
		if success && t1.type == .Assign {
			consume_token( parser );

			left := New(Identifier_Node);

			init_identifier_node(
				 left
				,t0.identifier
			);

			right := parse_expression( parser );

			binary_op := New(Binary_Operator_Node);
			init_binary_operator(
				  binary_op
				 ,.Assign
				 ,left
				 ,right
			);

		}
	}

}

// internal parsing functions that need to be used by the 
// parse_expression()
// parse_exterior_term()
// parse_exterior_factor()

#import "Basic";
