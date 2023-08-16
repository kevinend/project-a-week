/*
	This file implements a simple parser of Grassmann (exterior) algebra expressions.
	The structure of expressions handled by the parser are expressed as Backus-Naur Forms
	(BNF).

	As mentioned in the readme, Grassmann's products, especially the interior product of
	different graded elements is recursive in nature and cumbersome to expand by hand.
	This parser is written to allow an expansion phase to see the products fully expanded
	from an expression tree and then evaluated to return the final result.

	BNF for Grassmann expressions:
	 *The character R represents the 'Regressive' product. Don't want to get into unicode characters quite yet.
	  Eventually we will render those as logical and (U+2227) and or (U+2228) in unicode.
	 *The strings "LC" and "RC" represent the LEFT and RIGHT complement operations. 
	  Eventually will render those with the overline (U+203E) and underline (U+005F)

		expression 		::= <exterior-term>   [("+","-") <exterior-term]...
		exterior-term   ::= <exterior-factor> [("^","R") <exterior-factor>]...
		exterior-factor ::= ["-"] ["LC","RC"] ( <basis-one-element> | <number> |  | "(" <expression> ")" )

	NOTE:
	  Multiplication of basis elements by scalars is a tad clumsy.
	  You must specify multiplication using the exterior product. 3^e1 and not 3e1.
	  This isn't obvious until you try to parse the number 3 from 3e1 and use something like string-to-float
	  which e represents the exponent so it doesn't get read as a number with the additional e1 after it. It
	  gets read as 3e1 with the e interpreted as an exponent.

	  If your basis is not e1,e2,e3..eN then you could handle this differently but I don't want to restrict that
	  at this time so we are left with explicit exterior products.
*/

/*
	The code, as written, cannot handle k-vectors that are sums of basis elements where
	the grade of the k-vector > 1. 

	So a bivector in G(R3) == e1e2 + e1e3 can not be expressed as a single k-vector, it is
	getting added to a list as [e1e2|e1e3] though we may technically need to handle it as a single
	element (in the case of a span or cospan operation). Therefore, changing up the approach a bit here
	and want to introduce the idea of using the higher bits of the 'basis_elements' bit-vector to represent elements of grade > 1.

	Right now we use the lower bits; bits 0,1, and 2 to represent say e1, e2 and e3 in G(R3) and say that a bivector like e1e2 has
	both the e1 and e2 bits set. As mentioned above, this is not a technically feasible approach going forward so we want to use
	more bits from the 'basis_elements' bit-vector to represent higher-graded objects directly and NOT as a mish-mash of bits of the
	1-vectors.

	There is a scheme on the k-vectors that actually works out nicely but requires the use of offsets based on the grade.

	Easiest to show as an example. Assume G(R3) though this should work up to dimension 6 which has 64 basis-elements and we would run out of bits with a 64-bit integer.
	bits:      [0,   1,   2,   3,   4,    5,    6, ...]
	values:    [1,   2,   4,   8,   16,  32,   64, ...] // powers of two

	elements:  [e1, e2,  e3,  e12, e13, e23, e123, ...]
			   <---------->   <------------> <--->
			   	 1-vectors      2-vectors     3-vectors
			   ^Grade1-offset ^Grade2-offset ^Grade3-offset ...

	Setting the 'bits' indicates which basis elements are included in the k-vector. bit-0 means e1 is present, bit-1 means e2 is present, etc..
	The 'values' represent the integer value of that bit being set. These values are only important for the basis-1 elements in this scheme.

	Conceptually we divide the 'elements' array into sections based on the grade of the elements.
	So in the case of G(R3) we have a 3-element section for the 1-elements, followed by a 3-element section for the 2-elements followed by a 1-element section for the 3-elements.

	Those offsets are calculated a single time once we declare the dimension of the space and then we need to determine the bit-index for the element we are interested in.
	e1 is at bit-index 0 relative to it's starting offset, e1e2 is at bit-index 0 relative to it's starting offset and e1e2e3 is at bit-index 0 relative to it's starting offset.

	How do you calculate the bit-index for an element? The formula is the following.
		Bit-Index = (Value(Set-1-Bits)/Grade)-1


	Look at e1e2 which is the 4th bit (3rd index) in our bit-vector.
	If you set the bits, e1 and e2 the value of that integer is 3, if you divide this by the grade of the resulting element, 2 in this case, and subtract 1, you get the following:
	 	Bit-Index = (Sum/Grade)-1 = 3/2 - 1 = 0
    So if you offset into the 64-bit integer into the 2-vector 'section' you can set the 4th (index 3) bit and know that represents the value e1e2.

    After this example you assume that you can just sum the 1-vectors values and get the correct index but that assumption is incorrect.

    Look at e1e3 which is the 5th bit (4th index) in our bit-vector.
    Value(e1) + Value(e3) = 5, which is the correct bit but the incorrect index (should be 4).
    Applying the formula from above gives you:
    	Bit-Index = (Sum/Grade)-1 = (5/2)-1 = 1
    Which is the correct index that needs set once you offset into the Grade-2 basis elements section.
*/

/*
	TODO:
	xxx--Complement, need LC and RC that preceed terms, needs added to the distribution since LC(e1+e2) == LC(e1) + LC(e2)
	xxx--Replace eval with simplify and have eval call simplify at the root of the tree
	xxx--Negation doesn't work like i think it does, it only negates k-vectors and not sums so something like -(e1+e2) doesn't work, just haven't tested it yet which is why i don't know about it
	xxx--Need to create the Unary_Operator which is either complements or negation
	xxx--How to handle zero? If the answer is only zero then display otherwise exclude it from the output (i think), need this before writing the tests [filtered when flattening, removed zeroes]
	xxx--Need an init_k_element( grade, coefficient, basis )
	xxx--Pretty print the final expression for the test cases
	xxx--Remove clone_k_vector, clone_unary_operator, clone_binary_operator
	xxx--'As' to remove all the casting all over God's green earth
	xxx--Return results as a string and have the tests pass or fail based on the output, a simple string compare
	--Tokenizer for the front-end (required to do the symbol lookup)
	--New test cases:
		xxx-- 2D
		-- 3D
		-- 4D
		--Debugging tips on how to look at nodes in a tree
		--Symbol table so you can stash values, say you have a point P = e0 + e1 + e2, want to store that for reuse (then you have an interpreter effectively)
		--Less important but a stepwise expand, allow the user to click through and generate a string of the current state to see
	  	  how the process is working
	--Convert 'k-vector' into 'k-element' since we could have point spaces
	--DeMorgan's Law, I ALWAYS forget this and it's impossible to remember


*/

/*
Identifier
Keyword (these have to be defined ahead of time), expand, simplify, eval, k-span, k-cospan
Literal -- numbers effectively, coefficients
Separator, {, (, ), ;
Operator, +, -, RC, LC, R, ^, I

L1 = e0 + ae1 + be2; 
L2 = e0 + ae2 + be2;
L3 = L1 ^ L2;
eval( L3 );

*/



#scope_export

// globals:
dimension: int = 0;
basis_one_elements: [..] string;
n_element: int;

test_case: int = 0;

// Data Structures:

// Lexing:
Token_Kind :: enum {

	Undefined;

	Identifier;
	Literal; 
	Keyword;

	Equals;
	Plus;
	Minus;
	Exterior_Product;
	Regressive_Product;
	Interior_Product;
	Left_Complement;
	Right_Complement;

	Open_Paranthesis;
	Close_Paranthesis;
	Semicolon;
}

Token :: struct {
	kind: Token_Kind;	
}

Identifier_Token :: struct {
	#as using _kind: Token;
	value: string;
}

Literal_Token :: struct {
	#as using _kind: Token;
	value: string;
}

Keyword_Token :: struct {
	#as using _kind: Token;
	value: string;
}

Operator_Token :: struct {
	#as using _kind: Token;
}

Separator_Token :: struct {
	#as using _kind: Token;
}

// Parsing/Eval:
Node_Kind :: enum {
	K_Vector;
	Unary_Operator; 
	Binary_Operator; 
}

Expression_Node :: struct {
	kind: Node_Kind;
}

K_Vector_Node :: struct {
	// Handles scalars, and any k-vectors up to the dimension
	#as using _kind: Expression_Node;

	grade: int;
	coefficient: float;
	basis_components: s64; // bit field, we won't hit dimension 64 so I think we are okay here for now
}

Unary_Op :: enum {
	 Negation;
	 Left\_Complement;
	 Right_Complement;
}

Unary_Operator_Node :: struct {
	// negation, left and right complements
	#as using _kind: Expression_Node;

	operation: Unary_Op;
	operand:  *Expression_Node;
}

Binary_Op :: enum u8 {
	Sum;
	Difference;
	Exterior_Product;
	Regressive_Product;
}

Binary_Operator_Node :: struct {
	// exterior, regressive, and interior products, sums and differences 
	#as using _kind: Expression_Node;

	operation: Binary_Op;
	left_\operand: *Expression_Node;
	right_operand: *Expression_Node;
}

Complement_Variant :: enum {
	Left;
	Right;
}

// procedures:
skip_spaces :: ( s: string ) -> ( cursor: string ) {
	cursor := s;
	while cursor && (cursor[0] == #char " " || cursor[0] == #char "\n" || cursor[0] == 9) { // horizontal tab
		advance( *cursor, 1 );
	}
	return cursor;
}

is_whitespace :: ( s: string ) -> ( found: bool ) {
	cursor := s;
	if cursor[0] == #char " " || cursor[0] == #char "\n" || cursor[0] == 9 { // horizontal tab
		return true;
	}
	else {
		return false;
	}
}

string_copy :: ( s: string, n: int ) -> ( copy: string ) {

	copy: string;
	copy.data  = alloc( n );
	copy.count = s.count;
	memcpy( copy.data, s.data, n );

	return copy;
}

lex :: ( expression: string ) -> ( tokens: [..] *Token ) {

	generate_token :: ( lexeme: string ) -> ( token: *Token, token_length: int ) {

		get_operator :: ( lexeme: string ) -> ( token: *Token, match: bool, length: int ) {
			
			token_kind: Token_Kind = .Undefined;
			length: int = 0;
			if lexeme[0] == {
				case #char "="; { token_kind = .Equals; 			length = 1; }
				case #char "+"; { token_kind = .Plus;   		  	length = 1; }
				case #char "-"; { token_kind = .Minus; 			  	length = 1; }
				case #char "^"; { token_kind = .Exterior_Product;   length = 1; }
				case #char "R"; { token_kind = .Regressive_Product; length = 1; }
				case #char "I"; { token_kind = .Interior_Product;   length = 1; }
			}

			// for left and right complement need to compare the string values
			if string_compare( lexeme, "LC", 2 ) == 0 {
				token_kind = .Left_Complement;
				length = 2;
			}
			else
			if string_compare( lexeme, "RC", 2 ) == 0 {
				token_kind = .Right_Complement;
				length = 2;
			}

			if token_kind != .Undefined {
				token := New(Operator_Token);
				token.kind = token_kind;
				return token, true, length;
			}
			else {
				return null, false, 0;
			}
		}

		get_separator :: ( lexeme: string ) -> ( token: *Token, match: bool, length: int ) {
			
			token_kind: Token_Kind = .Undefined;
			length: int = 0;
			if lexeme[0] == {
				case #char "("; { token_kind = .Open_Paranthesis;   length = 1; }
				case #char ")"; { token_kind = .Close_Paranthesis;  length = 1; }
				case #char ";"; { token_kind = .Semicolon; 		    length = 1; }
			}

			if token_kind != .Undefined {
				token := New(Separator_Token);
				token.kind = token_kind;
				return token, true, length;
			}
			else {
				return null, false, 0;
			}		
		}

		get_literal :: ( lexeme: string ) -> ( token: *Token, match: bool, length: int ) {

			seek := lexeme;
			length: int = 0;
			if is_digit( seek[0] ) {
				advance( *seek, 1 );
				while seek && (is_digit( seek[0] ) || seek[0] == #char ".") {
					advance( *seek, 1 );	
				}

				length = seek.data - lexeme.data;

				token := New(Literal_Token);
				token.kind = .Literal;
				token.value = string_copy( lexeme, length );
				
				return token, true, length;
			}
			else
			if seek[0] == #char "\"" {
				advance( *seek, 1 );
				while seek && seek[0] != #char "\"" {
					advance( *seek, 1 );
				}

				length = seek.data - lexeme.data;

				token := New(Literal_Token);
				token.kind = .Literal;
				token.value = string_copy( lexeme, length );

				return token, true, length;
			}
			else {
				return null, false, 0;
			}
		}

		get_keyword :: ( lexeme: string ) -> ( token: *Token, match: bool, length: int ) {

			length: int = 0;
			
			if string_compare( lexeme, "set-basis", 9 ) == 0 {
				length = 9;

				token := New(Keyword_Token);
				token.kind = .Keyword;
				token.value = string_copy( lexeme, length );

				return token, true, length;
			}
			else
			if string_compare( lexeme, "set-dimension", 13 ) == 0 {
				length = 13;

				token := New(Keyword_Token);
				token.kind  = .Keyword;
				token.value = string_copy( lexeme, length );
				
				return token, true, length;
			}
			else {
				return null, false, length;
			}
		}

		get_identifier :: ( lexeme: string ) -> ( token: *Token, match: bool, length: int ) {
		
			length: int = lexeme.count;
			token := New(Identifier_Token);
			token.kind  = .Identifier;
			token.value = string_copy( lexeme, lexeme.count );

			return token, true, length;
		}
		
		token: *Token = null;
		match: bool = false;
		token_length: int = 0;

		token, match, token_length = get_operator( lexeme );
		if match { return token, token_length; }

		token, match, token_length = get_separator( lexeme );
		if match { return token, token_length; }

		token, match, token_length = get_literal( lexeme );
		if match { return token, token_length; }

		token, match, token_length = get_keyword( lexeme );
		if match { return token, token_length; }

		// catch-all for now
		// in this expression "3.14+(e1R7^e1)" from e1 onwards is being read as a single identifier
		token, match, token_length = get_identifier( lexeme );
		if match { return token, token_length; }
	}

	tokens: [..] *Token;
	token: *Token;
	token_length: int;
	cursor: string = expression;
	while cursor {
		cursor = skip_spaces( cursor );

		lexeme := cursor;
		while cursor && !is_whitespace( cursor ) {
			advance( *cursor, 1 );
		}
		lexeme.count = cursor.data - lexeme.data;

		// lexeme is a string with no whitespace characters, could contain multiple tokens.
		// example: lexeme = "3^e1" which is 3 separate tokens
		while lexeme {
			token, token_length = generate_token( lexeme );
			array_add( *tokens, token );

			advance( *lexeme, token_length );
		}
	}

	// so this is getting turned into '3^e1' '+' '2^e2'
	// to be lexed correctly it would need to be 3 ^ e1 + 2 ^ e2, which i do NOT like
	// assuming whitespace when it isn't there
	// do we need an additional step in here?
	// or do we need to split this apart and iteratively break this 'word' into it's constituent parts, that's another way to do it
	//
	// so two approaches then i need to actually 'work' ... bleh
	// (1) complicate this outer loop with logic to handle this 'word' boundary problem
	// (2) get the 'word' then do additional processing in the evaluator/generate_token step to break it apart into the constituent components
	//    so the proces would break into 'words' then to an inter-word processing on it (try that route for now)

/*
	for token: tokens {
		if token.kind == {
			case Token_Kind.Literal; 	  		{ print( "Literal\n" ); }
			case Token_Kind.Identifier; 		{ print( "Identifier\n" ); }
			case Token_Kind.Keyword;    		{ print( "Keyword\n"    ); }
			case Token_Kind.Exterior_Product;   { print( "Exterior Product\n"   ); }
			case Token_Kind.Plus;  				{ print( "Plus\n"  ); }
		}
	}
*/

	return tokens;
}

// lexer
//  scanner   	   (separate out the string)
//  generate_token (generate a token based on what you have there 
//   this can do three things 
//    (1) Generate a token with a value
//    (2) Generate a token without a value
//    (3) Suppress the string like comments and stuff and just get rid of it entirely

init_unary_operator :: ( node: *Unary_Operator_Node, operation: Unary_Op, operand: *Expression_Node ) {

	node.kind      = Node_Kind.Unary_Operator;
	node.operation = operation;
	node.operand   = operand;

	return;
}

init_binary_operator :: ( node: *Binary_Operator_Node, operation: Binary_Op, left: *Expression_Node, right: *Expression_Node ) {

	node.kind 	   	   = Node_Kind.Binary_Operator;
	node.operation 	   = operation;
	node.left\_operand = left;
	node.right_operand = right;

	return;
}

init_k_element :: ( node: *K_Vector_Node, grade: int, coefficient: float, basis_components: s64 ) {

	node.kind  		 	  = Node_Kind.K_Vector;
	node.grade 		 	  = grade;
	node.coefficient 	  = coefficient;
	node.basis_components = basis_components;

	return;
}


// parsing:


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

get_basis_one_element :: ( expression: string, basis_element_index: s64 ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := expression;

	basis_one_element := basis_one_elements[basis_element_index];

	node := New(K_Vector_Node);
	node.kind   	 = Node_Kind.K_Vector;
	node.grade  	 = 1; 
	node.coefficient = 1;
	node.basis_components |= (1 << basis_element_index);

	advance( *cursor, basis_one_element.count );

	return node, cursor;
}

parse_expression :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	term: *Expression_Node = null;

	term, cursor = parse_exterior_term( cursor );
	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "+" || cursor[0] == #char "-") {
	
		operation: Binary_Op;
		if cursor[0] == {
			case #char "+"; { operation = Binary_Op.Sum; }
			case #char "-"; { operation = Binary_Op.Difference; }
		}
		advance( *cursor, 1 );

		next_term: *Expression_Node;
		next_term, cursor = parse_exterior_term( cursor );

		node := New(Binary_Operator_Node);
		init_binary_operator( node, operation, term, next_term );

		term = node;
	}

	return term, cursor;
}

parse_exterior_term :: ( expression: string ) -> ( node: *Expression_Node, cursor: string ) {

	cursor := skip_spaces( expression );
	factor: *Expression_Node = null;

	factor, cursor = parse_exterior_factor( cursor );
	cursor = skip_spaces( cursor );

	while cursor && (cursor[0] == #char "^" || cursor[0] == #char "R") {
		
		operation: Binary_Op;
		if cursor[0] == {
			case #char "^"; { operation = Binary_Op.Exterior_Product; }
			case #char "R"; { operation = Binary_Op.Regressive_Product; }
		}

		advance( *cursor, 1 );

		next_factor: *Expression_Node;
		next_factor, cursor = parse_exterior_factor( cursor );

		node := New(Binary_Operator_Node);
		init_binary_operator( node, operation, factor, next_factor );

		factor = node;
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

	take_left\_complement: bool = false;
	take_right_complement: bool = false;
	if string_compare( cursor, "LC", 2 ) == 0 {
		take_left_complement = true;
		advance( *cursor, 2 );
	}
	else
	if string_compare( cursor, "RC", 2 ) == 0 {
		take_right_complement = true;
		advance( *cursor, 2 );
	}
 
	is_basis_one_element, basis_element_index := is_basis_one_element( cursor );

	if is_basis_one_element {
		node, cursor = get_basis_one_element( cursor, basis_element_index );
	}
	else
	if is_digit( cursor[0] ) {
		success: bool;
		value: float;
		value, success, cursor = string_to_float( cursor );

		scalar_node := New(K_Vector_Node);
		init_k_element( scalar_node, grade = 0, coefficient = value, basis_components = 0 );

		node = scalar_node;
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

	if factor_is_negative {
		parent_node := New(Unary_Operator_Node);
		init_unary_operator( parent_node, .Negation, node );

		node = parent_node;
	}

	if take_left_complement {
		parent_node := New(Unary_Operator_Node);
		init_unary_operator( parent_node, .Left_Complement, node );

		node = parent_node;
	}
	else
	if take_right_complement {
		parent_node := New(Unary_Operator_Node);
		init_unary_operator( parent_node, .Right_Complement, node );

		node = parent_node;
	}

	return node, cursor;
}

// expression-tree:

n_choose_k :: (n: int, k: int) -> (value: int) {
	// example: n_choose_k(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * n_choose_k(n-1, k-1)) / k;
}

set_dimension :: ( N: int ) {

	// Implmentation uses a single 64-bit integer to represent basis-one-elements.
	// Dimension must be a positive integer and less than 64.

	assert( N > 0  );
	assert( N < 64 ); 

	// Set the dimension
	dimension = N;

	// Build the n_element for the declared space
	for i: 0..N-1 {
		n_element |= (1 << i);
	}
		
	return;
}

set_basis_one_elements :: ( one_element_symbols: [] string ) {

	// TODO: check that there aren't duplicates passed in here by the caller

	assert( one_element_symbols.count == dimension );

	if basis_one_elements.count > 0 { array_reset( *basis_one_elements ); }

	// copy the symbols over into our global symbol/basis elements table
	for one_element_symbol: one_element_symbols {
		array_add(*basis_one_elements, one_element_symbol );
	}

	return;
}

clone_node :: ( node: *Expression_Node ) -> ( cloned_node: *Expression_Node ) {

	if node.kind == Node_Kind.K_Vector {
		
		k_vector := cast(*K_Vector_Node)node;

		cloned_node := New(K_Vector_Node);
		init_k_element( cloned_node, k_vector.grade, k_vector.coefficient, k_vector.basis_components );

		return cloned_node;
	}
	else
	if node.kind == Node_Kind.Unary_Operator {

		unary_operator := cast(*Unary_Operator_Node)node;

		cloned_node := New(Unary_Operator_Node);
		init_unary_operator( cloned_node, unary_operator.operation, clone_node( unary_operator.operand ) );

		return cloned_node;
	}
	else
	if node.kind == Node_Kind.Binary_Operator {

		binary_operator := cast(*Binary_Operator_Node)node;

		cloned_node := New(Binary_Operator_Node);
		init_binary_operator(
			 cloned_node
			,binary_operator.operation
			,clone_node( binary_operator.left\_operand )
			,clone_node( binary_operator.right_operand )
		);

		return cloned_node;
	}
	else {
		print( "Unrecognized node kind found when cloning nodes\n" );
		return null;
	}
}

is_grassmann_operator :: ( op: Binary_Op ) -> (bool) {

	if op == .Exterior_Product || op == .Regressive_Product {
		return true;
	}
	else {
		return false;
	}
}

distribute_grassmann_products :: ( node: *Expression_Node ) -> ( tree: *Expression_Node ) {

	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.
	 
	subtree: *Expression_Node;
	if node.kind == Node_Kind.K_Vector {
		subtree = clone_node( node );	
	}
	else 
	if node.kind == Node_Kind.Unary_Operator {
		unary_operator  := cast(*Unary_Operator_Node)node;

		// determine whether we need to distribute the operator or not
		need_to_distribute: bool = false;
		if unary_operator.operand.kind == Node_Kind.Binary_Operator {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;
			if binary_operator.operation == .Sum || binary_operator.operation == .Difference {
				need_to_distribute = true;
			}
		}

		if need_to_distribute {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;

			new_binary_operator := New(Binary_Operator_Node);
				
			new_left\_node := New(Unary_Operator_Node); 
			new_right_node := New(Unary_Operator_Node);

			init_unary_operator( new_left\_node, unary_operator.operation, clone_node( binary_operator.left\_operand ) );
			init_unary_operator( new_right_node, unary_operator.operation, clone_node( binary_operator.right_operand ) );

			init_binary_operator(
				 new_binary_operator
				,binary_operator.operation
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}	
	else {
		// Determine whether we need to distribute the binary grassmann products over any sums or differences.
		// The boolean values are introduced to reduce the level of nesting that happens if you don't 
		// do it that way (it was very hard to visually parse and a lot of duplicate 'clone the current node because
		// we don't need to distribute')

		binary_operator := cast(*Binary_Operator_Node)node;
		left\_operator: *Binary_Operator_Node = null;
		right_operator: *Binary_Operator_Node = null;

		distribute_over_left\_operand := false;
		distribute_over_right_operand := false;

		if is_grassmann_operator(binary_operator.operation) {
			if binary_operator.left_operand.kind == Node_Kind.Binary_Operator {
				left_operator = cast(*Binary_Operator_Node)binary_operator.left_operand;
				if left_operator.operation == .Sum || left_operator.operation == .Difference {
					distribute_over_left_operand = true;
				}
			}
			else if binary_operator.right_operand.kind == Node_Kind.Binary_Operator {
				right_operator = cast(*Binary_Operator_Node)binary_operator.right_operand;
				if right_operator.operation == .Sum || right_operator.operation == .Difference {
					distribute_over_right_operand = true;
				}	
			}
		}

		if distribute_over_left_operand {
			// distribute the argument on the left over the argument on the right
			grassmann_operator       := binary_operator.operation;
			distribute_over_operator := left_operator.operation;

			new_binary_operator := New(Binary_Operator_Node);

			new_left\_node := New(Binary_Operator_Node);
			new_right_node := New(Binary_Operator_Node);

			init_binary_operator(
				 new_left_node
				,grassmann_operator
				,clone_node( left_operator.left_operand )
				,clone_node( binary_operator.right_operand )
			);

			init_binary_operator(
				 new_right_node
				,grassmann_operator
				,clone_node( left_operator.right_operand )
				,clone_node( binary_operator.right_operand )
			);

			// to fully expand the subtrees we need to recursively call this procedure against the new left and right subtrees
			init_binary_operator(
				 new_binary_operator
				,distribute_over_operator // sum or difference
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else
		if distribute_over_right_operand {
			// distribute the argument on the left over the argument on the right
			grassmann_operator    	 := binary_operator.operation;
			distribute_over_operator := right_operator.operation;

			new_binary_operator := New(Binary_Operator_Node);

			new_\left_node := New(Binary_Operator_Node);
			new_right_node := New(Binary_Operator_Node);

			init_binary_operator(
				 new_left_node
				,grassmann_operator
				,clone_node( binary_operator.left_operand )
				,clone_node( right_operator.left_operand )
			);

			init_binary_operator(
				 new_right_node
				,grassmann_operator
				,clone_node( binary_operator.left_operand )
				,clone_node( right_operator.right_operand )
			);

			// to fully expand the subtrees we need to recursively call this procedure against the new left and right subtrees
			init_binary_operator(
				 new_binary_operator
				,distribute_over_operator // sum or difference
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}

	return subtree;
}

generate_k_span   :: ( element: *K_Vector_Node, k: int ) -> ( k_span: [..] *K_Vector_Node ) {
	
	// to generate a k-span you need the 1-span
	one_span: [..] int;
	for bit: 0..dimension-1 {
		if element.basis_components & (1 << bit) {
			array_add( *one_span, bit );
		}
	}

	k_span: [..] *K_Vector_Node;
	if k == 1 {
		for i: 0..one_span.count-1 {
			k_element := New(K_Vector_Node);
			init_k_element( k_element, grade = 1, coefficient = 1, basis_components = (1 << one_span[i]) );

			array_add( *k_span, k_element );
		}
	}
	else {
		// build the set of k-elements (k-combinations) from the one-span.
		// this is O(N^3), probably a better way to do this!
		for i: 0..one_span.count-1 {

			one_element := New(K_Vector_Node);
			init_k_element( one_element, grade = 1, coefficient = 1, basis_components = (1 << one_span[i]) );

			for j: i+1..one_span.count-1 {

				k_element := cast(*K_Vector_Node)clone_node( one_element );

				num_remaining_one_elements := one_span.count-j;
		
				if num_remaining_one_elements >= (k-1) {	
					s: int = 0;
					while k_element.grade != k {
						k_element.basis_components |= (1 << one_span[j+s]);
						k_element.grade += 1;
						s += 1;
					}

					array_add( *k_span, k_element );
				}
			}
		}
	}

	return k_span;
}

anticommutative_coefficient :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( coefficient: int ) {

	// This procedure calculates the number of swaps required to reorder the elements into the 'canonical' ordering.
	// If the number of swaps is odd then the product needs negated.

	// The current implementation forces an ordering of 1-elements such that the exterior product of (e2^e1)^e3
	// will automatically be reordered to the positive (scalar factor is positive) product e1^e2^e3.

	// Algorithm:
	// Take the exterior product between the elements and handle the anticommutativity property of the grassmann product.
	// For every bit (basis one element) in the LEFT argument
	//	Start at right-most bit in the LEFT arg
	//  Find it's place in the RIGHT arg and set that bit.
	//	Anytime you cross over a bit in the RIGHT that is set, then you have swapped the elements.
	// Rinse and repeat for every other bit in the LEFT.
	//
	// If after completing this process we have swapped an odd number of times then we negate the final product.

	temp: int = rvalue.basis_components;
	num_swaps: int = 0;
	for < i: dimension-1..0 {
		if lvalue.basis_components & (1 << i) {
			for j: 0..dimension-1 {
				if i == j { 
					temp |= (1 << j);
					break;
				}
				else 
				if temp & (1 << j) { 
					num_swaps += 1;
				}
			}
		}
	}

	if num_swaps % 2 == 0 { return 1; }
	else { return -1; }
}

generate_k_cospan :: ( m_element: *K_Vector_Node, k_span: [..] *K_Vector_Node ) -> ( k_cospan: [..] *K_Vector_Node ) {

	co_m_element :: ( m_element: *K_Vector_Node, k_element: *K_Vector_Node ) -> ( m_k_element: *K_Vector_Node ) {

		// similar to the 'complement' however this is with respect to an m-element instead of the basis n-element.
		
		m_k_element := New(K_Vector_Node);
		init_k_element( 
			 m_k_element
			,grade = m_element.grade - k_element.grade
			,coefficient = 1
			,basis_components = m_element.basis_components ^ k_element.basis_components 
		);

		m_k_element.coefficient *= anticommutative_coefficient( k_element, m_k_element );

		return m_k_element;
	}

	k_cospan: [..] *K_Vector_Node;
	for k_element: k_span {
		m_k_element := co_m_element( m_element, k_element );
		array_add( *k_cospan, m_k_element );
	}

	return k_cospan;
}

simplify :: ( node: *Expression_Node ) -> ( result: *Expression_Node ) {

	// This procedure can be called on any node in an expression tree.
	// The reason it can't always return a k-vector is that sums of k-vectors of different grades are not defined.

	result: *Expression_Node;
	if node.kind == Node_Kind.K_Vector {	
		result = clone_node( node );
	}
	else
	if node.kind == Node_Kind.Unary_Operator {
		unary_operator := cast(*Unary_Operator_Node)node;
		unary_operator.operand = simplify( unary_operator.operand );

		assert( unary_operator.operand.kind == Node_Kind.K_Vector );

		operand := cast(*K_Vector_Node)unary_operator.operand;

		if unary_operator.operation == .Left_Complement {
			result = complement( Complement_Variant.Left, operand );
		}
		else
		if unary_operator.operation == .Right_Complement {
			result = complement( Complement_Variant.Right, operand );
		}
		else 
		if unary_operator.operation == .Negation {
			operand.coefficient *= -1;
			result = clone_node( operand );
		}
	}
	else {
		binary_operator := cast(*Binary_Operator_Node)node;
		binary_operator.left\_operand = simplify( binary_operator.left\_operand );
		binary_operator.right_operand = simplify( binary_operator.right_operand );

		if binary_operator.left\_operand.kind == Node_Kind.K_Vector && binary_operator.right_operand.kind == Node_Kind.K_Vector {
			left  := cast(*K_Vector_Node)binary_operator.left\_operand;
			right := cast(*K_Vector_Node)binary_operator.right_operand;

			operation := binary_operator.operation;
			if operation == .Exterior_Product {
				result = exterior_product( left, right );
			}
			else
			if operation == .Regressive_Product {
				result = regressive_product( left, right );
			}
			else 
			if operation == .Sum || operation == .Difference {
				// sum( binary_operator, left, right )
				// if left.grade != right.grade {
				// 
				// }
				// else
				// we can just generate a new one, pass in the left and right, if we can't return a new binary operator
				if left.grade == right.grade && left.basis_components == right.basis_components {
					sum := cast(*K_Vector_Node)clone_node( left );
					if operation == .Difference { right.coefficient *= -1; } // part of expand could be converting differences into sums
					sum.coefficient += right.coefficient;
					result = sum;
				}
				// zero is the additive identity
				else
				if left.coefficient == 0 {
					sum := cast(*K_Vector_Node)clone_node( right );
					result = sum;
				}
				else
				if right.coefficient == 0 {
					sum := cast(*K_Vector_Node)clone_node( left );
					result = sum;
				}
				else {
					// sum is of two k-vectors of different grade; can't simplify.
					result = clone_node( node );
				}
			}
		}
		else {
			// Clone the node. One node is a sum and we can't simplify it further.
			result = clone_node( node );
		}
	}
	
	return result;
} 

// TODO: Combine this and 'distribute_grassmann_products' into a single 'expand' function
apply_common_factor_theorem :: ( node: *Expression_Node ) -> ( tree: *Expression_Node ) {

	subtree: *Expression_Node;
	if node.kind != Node_Kind.Binary_Operator {
		subtree = clone_node( node );
	}
	else {
		// Determine whether we need to apply the common factor theorem to any regressive products.
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.operation == .Regressive_Product {
			left  := cast(*K_Vector_Node)simplify( binary_operator.left\_operand );
			right := cast(*K_Vector_Node)simplify( binary_operator.right_operand );

			if (left.grade + right.grade) < dimension {
				// regressive product is zero, don't bother expanding
				subtree = clone_node( node );
			}
			else {
				// generate the k-span and k-cospan of the right node.
				// k represents the grade of the element the left and right share in common.
				// Example:
				//	 N = 4
				//   A = e1^e3
				//   B = e2^e3^e4
				//   k = (Grade(A) + Grade(B)) - N == (2+3) - 4 == 5-4 = 1
				// 1-span:	 [e2,e3,e4]
				// 1-cospan: [e3e4, -e2e4, e2e3]

				k := (left.grade + right.grade) - dimension;
				k_span:	  [..] *K_Vector_Node = generate_k_span\ ( right, k );
				k_cospan: [..] *K_Vector_Node = generate_k_cospan( right, k_span );

				// build the regressive product subtrees from the k-span and k-cospan elements.
				// Example: using the span and cospan from the above example
				// regressive_product_subtrees = [subtree0, subtree1, subtree2, ...]
				//   where subtree0 =		R
				//						^     k-span[0]
				//				 	  L  k-cospan[0]

				regressive_product_subtrees: [..] *Binary_Operator_Node;
				for i: 0..k_span.count-1 {
				
					exterior_product_node := New(Binary_Operator_Node);
					init_binary_operator( 
						 exterior_product_node
						,.Exterior_Product
						,clone_node( left )
						,k_cospan[i] 
					);

					regressive_product_node := New(Binary_Operator_Node);
					init_binary_operator(
						 regressive_product_node
						,.Regressive_Product
						,exterior_product_node
						,k_span[i]
					);

					array_add( *regressive_product_subtrees, regressive_product_node );
				}

				// generate the new root node with the sums of the regressive product subtrees created above
				// Example: convert the subtrees into a tree 'sum', provided there is more than one subtree
				// regressive_product_subtrees: [subtree0, subtree1, subtree2, subtree3]
				//      		+
				// 		subtree0    +
				//      	subtree1 	+
				//        		subtree2 subtree3


				if regressive_product_subtrees.count == 1 {
					subtree = regressive_product_subtrees[0];
				}
				else {

					root := New(Binary_Operator_Node);
					init_binary_operator( root, .Sum, null, null );
					
					i: int = 0;
					current := root;
					while i < regressive_product_subtrees.count-2 { 
						current.left_operand = regressive_product_subtrees[i];
						
						next := New(Binary_Operator_Node);
						init_binary_operator( next, .Sum, null, null );

						current.right_operand = next;
						current = next;

						i += 1;
					}

					// the final sum needs handled outside the loop
					current.left\_operand = regressive_product_subtrees[i];
					current.right_operand = regressive_product_subtrees[i+1];

					subtree = root;
				}
			}
		}
		else {
			// binary operator node is not a regressive product, just clone the node and return it
			subtree = clone_node( node );
		}
	}

	return subtree;
}

complement :: ( variant: Complement_Variant, k_vector: *K_Vector_Node ) -> ( n_k_vector: *K_Vector_Node ) {
	
	n_k_vector := New(K_Vector_Node);
	init_k_element(
		 n_k_vector
		,grade 		 	  = dimension - k_vector.grade
		,coefficient 	  = k_vector.coefficient
		,basis_components = n_element ^ k_vector.basis_components
	);

	// Need to determine whether the (n-k)-element should be negated.
	// The procedure below effectively takes the exterior product between the args and determines the number
	// of swaps required to build the final N element from the product of the (n-k)-element or the k-element
	// or vice versa depending on whether this is a left or right complement.

	if variant == Complement_Variant.Left {
		n_k_vector.coefficient *= anticommutative_coefficient( n_k_vector, k_vector );
	} 
	else
	if variant == Complement_Variant.Right {
		n_k_vector.coefficient *= anticommutative_coefficient( k_vector, n_k_vector );
	}

	return n_k_vector;
}

exterior_product :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( result: *K_Vector_Node ) {

	// assume the result is zero, change it if it isn't the case
	result := New(K_Vector_Node);
	init_k_element( result, grade = 0, coefficient = 0, basis_components = 0 );

	if lvalue.grade == 0 || rvalue.grade == 0 {
		result.grade       	    = lvalue.grade + rvalue.grade;
		result.coefficient 	    = lvalue.coefficient * rvalue.coefficient;	
		result.basis_components = lvalue.basis_components | rvalue.basis_components;
	}
	else
	if (lvalue.basis_components & rvalue.basis_components) == 0 { // no one elements in common; implies sum of grades is less than or equal to N	
		result.grade 			= lvalue.grade + rvalue.grade;
		result.coefficient  	= lvalue.coefficient * rvalue.coefficient;
		result.basis_components = lvalue.basis_components | rvalue.basis_components;

		result.coefficient *= anticommutative_coefficient( lvalue, rvalue );
	}

	return result;
}

regressive_product :: ( lvalue: *K_Vector_Node, rvalue: *K_Vector_Node ) -> ( result: *K_Vector_Node ) {

	// assume the result is zero, change it if it isn't the case
	result := New(K_Vector_Node);
	init_k_element( result, grade = 0, coefficient = 0, basis_components = 0 );

	if lvalue.grade == dimension {
		// return the right-operand 
		result.grade 	   		= rvalue.grade;
		result.coefficient 		= rvalue.coefficient * lvalue.coefficient;
		result.basis_components = rvalue.basis_components;
	}
	else 
	if rvalue.grade == dimension {
		// return the left-operand
		result.grade 			= lvalue.grade;
		result.coefficient 		= lvalue.coefficient * rvalue.coefficient;
		result.basis_components = rvalue.basis_components; 
	}

	return result;
}

flatten :: ( subtree: *Expression_Node ) -> ( elements: [..] K_Vector_Node ) {

	// This procedure returns a flat array of k-elements that are non-zero.
	// At this point simplify has executed from the root and we are either left with a single k-element or a sum of k-elements.
	// If the element has a zero coefficient we exclude it from the final output. The conversion to a string takes care of the
	// case when the final result is zero.

	helper :: ( node: *Expression_Node, elements: *[..] K_Vector_Node ) {

		if node.kind == Node_Kind.K_Vector {
			k_vector_node := cast(*K_Vector_Node)node;

			k_vector: K_Vector_Node;
			init_k_element( *k_vector, k_vector_node.grade, k_vector_node.coefficient, k_vector_node.basis_components );

			if k_vector_node.coefficient != 0 {  // float-compare!!!
				array_add( elements, k_vector );	
			}
		}
		else
		if node.kind == Node_Kind.Binary_Operator {
			binary_operator := cast(*Binary_Operator_Node)node;
			helper( binary_operator.left\_operand, elements );
			helper( binary_operator.right_operand, elements );
		}

		return;
	}

	elements: [..] K_Vector_Node;
	helper( subtree, *elements );
	
	return elements;
}


// TODO: Clean this up, this is crazy territory here just to print sums and differences
//       Remember this gets called by the debug_print procedure, maybe make your own version and embed in there? 
k_element_to_string ::  ( k_element: K_Vector_Node, pretty_coefficients: bool = false ) -> ( result: string ) {

	builder: String_Builder;
	num_processed_basis_one_elements: int = 0;

	coefficient: string; 
	if pretty_coefficients {
		if k_element.grade > 0 {
			if k_element.coefficient != 1.0 && k_element.coefficient != -1.0 {
				if k_element.coefficient < 0 { coefficient = tprint( "%", k_element.coefficient * -1 ); }
				else					     { coefficient = tprint( "%", k_element.coefficient ); }
			}
			// don't display the coefficient if it is a value of 1 or -1
		}
		else {
			// display any coefficient because this is a scalar
			if k_element.coefficient < 0 { coefficient = tprint( "%", k_element.coefficient * -1 ); }
			else						 { coefficient = tprint( "%", k_element.coefficient ); }
		}
		
		append( *builder, coefficient );
	}
	else {
		coefficient = tprint( "%", k_element.coefficient ); 
		append( *builder, coefficient );
	}

	for i: 0..dimension-1 {
		if k_element.basis_components & (1 << i) {
			append( *builder, basis_one_elements[i] );
			if num_processed_basis_one_elements < k_element.grade-1 { // in an expression like e1^e2 the number of exterior products is the grade-1
				append( *builder, "^" );
			}

			num_processed_basis_one_elements += 1;
		}
	}
	
	// TODO: Leaking memory on the tprint and the builder
	s: string = builder_to_string( *builder );
	
	return s;
}

// TODO: Revisit this and the procedure for converting the k-element to strings, this is so messy to do something trivial
elements_to_expression :: ( elements: [] K_Vector_Node ) -> ( expression: string ) {

	// Converts the array of k-elements into a single string expression.
	// The 'flatten' procedure filters out zero k-elements.
	// If the tree was either a single zero element or the sum of zero elements our list of elements is empty.
	// In that case we print a single zero.

	expression_builder: String_Builder;

	if elements.count == 0 {
		append( *expression_builder, "0" );
	}
	else {
		if elements[0].coefficient < 0 {
			append( *expression_builder, "-" );
		}
		append( *expression_builder, k_element_to_string( elements[0], pretty_coefficients = true ) );

		for i: 1..elements.count-1 {
			if elements[i].coefficient < 0 { append( *expression_builder, " - " ); }
			else  						   { append( *expression_builder, " + " ); }
			append( *expression_builder, k_element_to_string( elements[i], pretty_coefficients = true ) );
		}
	}
	
	// TODO: Free builder, leaking memory...
	expression: string = builder_to_string( *expression_builder );

	return expression;
}

eval :: ( expression: string ) -> ( result: string ) {

	expression_tree: *Expression_Node;
	expression_tree = parse_expression( expression );
	expression_tree = distribute_grassmann_products( expression_tree );
	expression_tree = apply_common_factor_theorem( expression_tree );

	expression_tree = simplify( expression_tree );
	
	elements: [..] K_Vector_Node;
	elements = flatten( expression_tree );
	
	result: string;
	result = elements_to_expression( elements );

	return result;
}

debug_print :: ( expression: string ) {

	print_expression_tree :: ( node: *Expression_Node, indent: int = 0 ) {

		for i: 0..indent-1 {
			print( " " );
		}

		if node.kind == Node_Kind.K_Vector {
			
			k_vector := cast(*K_Vector_Node)node;

			k_vector_as_string := k_element_to_string( k_vector );
			print( "%\n", k_vector_as_string );
		}
		else 
		if node.kind == Node_Kind.Unary_Operator {
			unary_operator := cast(*Unary_Operator_Node)node;
			
			if unary_operator.operation == {
				case .Negation; 		{ print( "-\n"  ); }
				case .Left\_Complement; { print( "LC\n" ); }
				case .Right_Complement; { print( "RC\n" ); }
			}

			print_expression_tree( unary_operator.operand, indent + 2 );
		}
		else 
		if node.kind == Node_Kind.Binary_Operator {
			binary_operator := cast(*Binary_Operator_Node)node;
			
			if binary_operator.operation == {
				case .Exterior_Product; 	{ print( "^\n" ); }
				case .Regressive_Product; 	{ print( "R\n" ); }
				case .Sum;					{ print( "+\n" ); }
				case .Difference; 			{ print( "-\n" ); }
				case; print( "Unhandled operator case in print procedure.\n" );
			}

			print_expression_tree( binary_operator.left\_operand, indent + 2 );
			print_expression_tree( binary_operator.right_operand, indent + 2 );
		}

		return;
	}

	print( "Debug Printing of Expression: %\n", expression );

	expression_tree: *Expression_Node;
	expression_tree = parse_expression( expression );

	print( "Initial expression tree\n" );
	print_expression_tree( expression_tree );
	print( "\n" );

	expression_tree = distribute_grassmann_products( expression_tree );
	print( "Expression tree post distribution of grassmann products:\n" );
	print_expression_tree( expression_tree );
	print( "\n" );

	expression_tree = apply_common_factor_theorem( expression_tree );
	print( "Expression tree post application of the common factor theorem:\n" );
	print_expression_tree( expression_tree );
	print( "\n" );

	expression_tree = simplify( expression_tree );

	elements: [..] K_Vector_Node;
	elements = flatten( expression_tree );

	result: string;
	result = elements_to_expression( elements );

	print( "Simplified expression:\n" );
	print( "%\n", result );

	return;
}

run_tests :: () {

	// 2D tests
	{
		set_dimension( N = 2 );
		set_basis_one_elements( string.["e1","e2"] );

		expression: string;
		result:     string;

		// scalars
		expression = "1";
		result = eval( expression );
		assert( result == "1" );

		expression = "-1";
		result = eval( expression );
		assert( result == "-1" );

		expression = "1+1";
		result = eval( expression );
		assert( result == "2" );

		expression = "1-1";
		result = eval( expression );
		assert( result == "0" );

		expression = "1+-1";
		result = eval( expression );
		assert( result == "0" );

		expression = "1+(1)";
		result = eval( expression );
		assert( result == "2" );

		expression = "(1)+1";
		result = eval( expression );
		assert( result == "2" );

		expression = "(1+1)";
		result = eval( expression );
		assert( result == "2" );

		expression = "1^1";
		result = eval( expression );
		assert( result == "1" );

		expression = "1^2";
		result = eval( expression );
		assert( result == "2" );

		expression = "1^2^3";
		result = eval( expression );
		assert( result == "6" );

		expression = "1+(2^3)";
		result = eval( expression );
		assert( result == "7" );

		expression = "1^(2+3)";
		result = eval( expression );
		assert( result == "5" );

		expression = "(1+2)^(3+4)";
		result = eval( expression );
		assert( result == "21" );

		expression = "(1+2)^(2+(3^4))";
		result = eval( expression );
		assert( result == "42" );

		// basis elements
		expression = "e1";
		result = eval( expression );
		assert( result == "e1" );

		expression = "-e1";
		result = eval( expression );
		assert( result == "-e1" );

		expression = "e1-e1";
		result = eval( expression );
		assert( result == "0" );

		expression = "e1+e1";
		result = eval( expression );
		assert( result == "2e1" );

		expression = "-(e1+e2)";
		result = eval( expression );
		assert( result == "-e1 - e2" );

		expression = "1^e1";
		result = eval( expression );
		assert( result == "e1" );

		expression = "e1^1";
		result = eval( expression );
		assert( result == "e1" );

		expression = "0^e1";
		result = eval( expression );
		assert( result == "0" );

		expression = "e1^0";
		result = eval( expression );
		assert( result == "0" );

		expression = "e1^e1";
		result = eval( expression );
		assert( result == "0" );

		expression = "e1^e2";
		result = eval( expression );
		assert( result == "e1^e2" );

		expression = "e1^(e1+e2)";
		result = eval( expression );
		assert( result == "e1^e2" );

		expression = "(e1+e2)^(e1+e2)";
		result = eval( expression );
		assert( result == "0" );

		expression = "LC(e1)";
		result = eval( expression );
		assert( result == "-e2" );

		expression = "LC(e2)";
		result = eval( expression );
		assert( result == "e1" );

		expression = "RC(e1)";
		result = eval( expression );
		assert( result == "e2" );

		expression = "RC(e2)";
		result = eval( expression );
		assert( result == "-e1" );

		expression = "RC(e1+e2)";
		result = eval( expression );
		assert( result == "e2 - e1" );

		expression = "LC(e1+e2)";
		result = eval( expression );
		assert( result == "-e2 + e1" );

		expression = "RC(e1^e2)";
	    result = eval( expression );
        assert( result == "1" );

		expression = "LC(e1^e2)";
	    result = eval( expression );
        assert( result == "1" );

		expression = "LC(e1)^RC(e2)"; 
	    result = eval( expression );
        assert( result == "-e1^e2" );
	}

	return;
}

// so this actually has to be a console application with a while loop and everything
// and then we runtime interpret certain parts of it
//

// so typically you have a program that is in a file and you do something like 
//  lex
//  parse
//  typecheck
//  link
//  generate executable file
//
// but in this case we want an interactive application which needs to loop
// so the loop goes something like
//  Enter information
//  Hit enter
//  Scan for keywords
//  execute those keywords
//  eval will trigger the token generation, ast generation, application and evaluation or something like that

// how cool i stumbled across his example which is a sample lexer that is much more involved than the one i was writing
// he solved that problem in kind of a unique way i thought
// atom1 --> point these to keywords effectively (whatever an atom is)
// atom2 --> 
// atom3 -->
//
// the tokens he did were simple too
// KEYWORD_IF
// KEYWORD_SET_DIMENSION()
// KEYWORD_SET_BASIS()
//
// also tracks the start and end of the Token from the input
// Line0, Character0
// Line1, Character1

// he did it based on length which might work
// if length == {
//   case 9; 
//		if name == set_basis
//   case 12;
//		if name == set_dimension


main :: () {

	// first step is to try and make this whole thing interactive, so it has to be a loop
	input: string;
	i: int = 0;
	while input != "exit" {
		if i == 5 { break; }
		print( "Running!" );
		i += 1;
	}

	s: string = "Hello Worldstar";
	lex( s );

	s = "3^e1 + 2^e2";
	lex( s );

	s = "set-basis();";
	tokens := lex( s );
	print( "Debug\n" );

	s = "3";
	tokens = lex( s );
	print( "Debug\n" );

	s = "3.14+(e1R7^e1);";
	tokens = lex( s );
	print( "Debug" );

	print( "Running tests...\n" );
	run_tests();
	print( "Test execution completed!\n" );

// test N=3 RC(e1)
// test N=3 RC(e2)
// test N=3 RC(e1^e3)
// test N=2 RC(e2)
// test N=2 RC(e1)
// test N=4 RC(e1^e2)
// test N=4 RC(e1)
// test N=4 RC(e2)
// test N=4 RC(e1+(e3+e2))
// test N=4 RC(e1) + e2
// test N=4 k-span(e1^e2^e3^e4, 1) // this is where the 'tokenizer' will be important
// test N=4 k-span(e1^e2^e3^e4, 2)
// test N=4 k-span(e1^e2^e3^e4, 3)
// test N=4 k-span(e1^e2^e3^e4, 4)
// test N=4 k-cospan(e1^e2^e3^e4, 1) // M-1 elements for the next couple
// test N=4 k-cospan(e1^e2^e3^e4, 2)
// test N=4 k-cospan(e1^e2^e3^e4, 3)
// test N=4 k-cospan(e1^e2^e3^e4, 4)
// test N=4 (e2^e3)R(e1^e3^e4)
// test N=5,(e1^e2^e3)+(e2^e3^e4)+(e1^e4^e5)
// test N=3, "3"
// test N=3, "3^3" // the sum might be broken, that's okay though
// test N=3, "e1^e2"
// test N=3, "3+3"
// test N=3, "3+3+3"
// test N=3, "e2^e1"
// test N=3, "e1^(e1+e2)"
// test N=3, "(e1+e2)^(e1+e2)"
// test N=3, "e1 + e2 + e1"
// test N=3, "3+e1+3"
// test N=3, "e1^(e1+e2+e3)"
// test N=3, "(e1+e2)^(e1+e2+e3)" 
// test N=3, "3" 
// test N=3, "-3" 
// test N=3, "3^5" 
// test N=3, "-3^-5"
// test N=3, "3^e1"
// test N=3, "-3^e1"
// test N=3, "e1^e1"
// test N=3, "e1^e2"	
// test N=3, "(e1^e2)"
// test N=3, "(e1^e2)^e3"
// test N=3, "e1^e2^e3"
// test N=3, "(3^e1)^(4^e2)"	
// test N=3, "e1+e1"
// test N=3, "e1-e1"
// test N=3, "e1+e2+e1" 
// test N=3, "e1-2^e1"
// test N=3, "e1^e2+e1"
// test N=3, "e1^(e1+e2)" 
// test N=3, "(e1+e3)^(e1+e2)" 


/*

	// THESE SHOULD BE TESTS AS WELL!
	init_dimension( N = 4 );
	init_basis_one_elements( string.["e1","e2","e3","e4"] );

	expression: string = "e1";
	result: string = eval( expression );
	print( "%\n", result );

	debug_print( expression );

	expression = "3";
	result = eval( expression );
	print( "%\n", result );

	expression = "3^3";
	result = eval( expression );
	print( "%\n", result );

	expression = "-e1";
	result = eval( expression );
	print( "%\n", result );

	expression = "-(e1+e2)";
	result = eval( expression );
	print( "%\n", result );

	expression = "e2";
	result = eval( expression );
	print( "%\n", result );

	expression = "e3";
	result = eval( expression );
	print( "%\n", result );

	expression = "e4";
	result = eval( expression );
	print( "%\n", result );

	expression = "e1^e2";
	result = eval( expression );
	print( "%\n", result );

	expression = "e1^e1";  // this is zero and not sure we represent that at all since we don't have coefficients
	result = eval( expression );
	print( "%\n", result );

	expression = "e1+e2";  // this is zero and not sure we represent that at all since we don't have coefficients
	result = eval( expression );
	print( "%\n", result );

	expression = "e1+e2+e3";  // this is zero and not sure we represent that at all since we don't have coefficients
	result = eval( expression );
	print( "%\n", result );

	expression = "e1^(e1+e2)";  // printing 1 in there but oh well
	result = eval( expression );
	print( "%\n", result );

	expression = "LC(e1)"; 
	result = eval( expression );
	print( "%\n", result );

	expression = "RC(e1)"; 
	result = eval( expression );
	print( "%\n", result );

	expression = "(e2^e3)R(e1^e3^e4)";  // cool it works but we still have the 'zero' problem
	result = eval( expression );
	print( "%\n", result );

	debug_print( expression );
*/

	return;
}

#scope_file
#import "Basic";
