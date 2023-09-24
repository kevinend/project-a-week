#scope_export

Lexer :: struct {

	MAX_NUM_TOKENS_PER_EXPRESSION :: 100;  // an expression is a single line of input
	TEMP_BUFFER_LENGTH      	  ::  64; // dictates the size of identifiers and the number of digits in a number

	current_line_number: 	 Line_Number     = 0;
	current_character_index: Character_Index = 0;

	input: string;
	input_cursor: int;

	tokens: [MAX_NUM_TOKENS_PER_EXPRESSION] Token;
	num_tokens: int = 0;

	temp_buffer: [TEMP_BUFFER_LENGTH] u8;
}


lexer_set_input_from_string :: ( lexer: *Lexer, user_input: string ) {

	lexer.current_character_index = 0;

	lexer.input = user_input;
	lexer.input_cursor = 0;

	return;
}

lexer_generate_tokens :: ( lexer: *Lexer, one_element_symbols: [] string ) {

	clear_tokens :: ( lexer: *Lexer ) {
		// Prior line's tokens get overwritten on every new input line.
		// This could change in the future!
		lexer.num_tokens = 0;
	}

	c0: u8;
	success: bool;
	token: Token;

	clear_tokens( lexer );

	while true {  	
	 	
	 	success, c0 = peek_next_character( lexer );
		while success && is_space( c0 ) { // is_space is from JAI library; Basic???
			consume_character( lexer );
			success, c0 = peek_next_character( lexer );
		}

		if !is_remaining_input( lexer ) {
			token = compose_end_of_input_token();
			add_token( lexer, token );
			break;
		}

		if begins_identifier( c0 ) {
			token = compose_identifier_keyword_or_basis_symbol_token( lexer, one_element_symbols );
			add_token( lexer, token );
			continue;	
		}

		if begins_number( c0 ) {
			token = compose_number_token( lexer );
			add_token( lexer, token );
			continue;
		} 

		c1: u8;
		if c0 == {
			// separators
			case #char ";"; { 
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Semicolon );
				add_token( lexer, token );
			}
			case #char "("; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Open_Paren );
				add_token( lexer, token );
			}
			case #char ")"; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Close_Paren );
				add_token( lexer, token );
			}
			case #char "["; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Open_Square_Brace );
				add_token( lexer, token );
			}
			case #char "]"; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Close_Square_Brace );
				add_token( lexer, token );
			}
			case #char ","; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Comma );
				add_token( lexer, token );
			}

			// operators
			case #char "+"; { 
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Plus );
				add_token( lexer, token );
			}
			case #char "-"; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Minus );
				add_token( lexer, token );
			}
			case #char "="; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Assign );
				add_token( lexer, token );
			}
			case #char "^"; { 
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Exterior_Product ); 
				add_token( lexer, token );
			}
			case #char "!"; {
				// !^ is the regressive product (as in 'not the exterior product)
				consume_character( lexer );

				success, c1 = peek_next_character( lexer );
				
				if success && c1 == #char "^" {
					consume_character( lexer );
					token = compose_separator_or_operator_token( .Regressive_Product );
					add_token( lexer, token );
				}
				// error here? if !success there is nothing left in the input and that character isn't valid
			}
			case #char "@"; {
				consume_character( lexer );
				token = compose_separator_or_operator_token( .Interior_Product );
				add_token( lexer, token );
			}
			case #char "<"; {
				// <<  is the left complement for now
				consume_character( lexer );

				success, c1 = peek_next_character( lexer );

				if success && c1 == #char "<" {
					consume_character( lexer );
					token = compose_separator_or_operator_token( .Left_Complement );
					add_token( lexer, token );
				}
				// error here?  if !success then there isn't another character and there should be
			}
			case #char ">"; {
				// >> is the right complement for now
				consume_character( lexer );

				success, c1 = peek_next_character( lexer );

				if success && c1 == #char ">" {
					consume_character( lexer );
					token = compose_separator_or_operator_token( .Right_Complement );
					add_token( lexer, token );
				}
				// error here? if !success then there isn't another character and there should be
			}
		}
	}

	return;	
}

lexer_contains_no_tokens :: ( lexer: *Lexer ) -> ( yes_no: bool ) {

	return lexer.tokens[0].type == .End_Of_Input;
}

print_tokens :: ( lexer: *Lexer, header: string ) {
	
	print( "%\n", header );

	for i: 0..lexer.num_tokens-1 {
		token := lexer.tokens[i];
		if token.type == {
			case .Error;              		{ print( "ERROR\n" ); }
			case .End_Of_Input;       		{ print( "END_OF_INPUT\n" ); }
			case .Semicolon; 		  		{ print( "SEMICOLON\n" );  }
			case .Open_Paren;         		{ print( "OPEN_PAREN\n" ); }
			case .Close_Paren; 		  		{ print( "CLOSE_PAREN\n" );}
			case .Open_Square_Brace;  	  	{ print( "OPEN_SQUARE_BRACE\n" ); }
			case .Close_Square_Brace; 	  	{ print( "CLOSE_SQUARE_BRACE\n" ); }
			case .Comma;			  	  	{ print( "COMMA\n" ); }
			case .Identifier; 		  	  	{ print( "Token == %\n", token.identifier ); }
			case .Keyword_Get;				{ print( "KEYWORD_GET\n" ); }
			case .Keyword_Set;				{ print( "KEYWORD_SET\n" ); }
			case .Keyword_Exit;				{ print( "KEYWORD_EXIT\n" ); }
			case .Keyword_Dimension;      	{ print( "KEYWORD_DIMENSION\n" ); }
			case .Keyword_Basis; 			{ print( "KEYWORD_BASIS\n" ); }
			case .Basis_Element;          	{ print( "Basis elements %\n", token.basis ); }
			case .Number;     		  		{ print( "Token == %\n", token.number ); }
			case .Plus; 		 	  		{ print( "PLUS\n"  ); }
			case .Minus;      		  		{ print( "MINUS\n" ); }
			case .Assign;             		{ print( "ASSIGN\n" ); }
			case .Exterior_Product;   		{ print( "EXTERIOR\n" ); }
			case .Regressive_Product; 		{ print( "REGRESSIVE\n" ); }
			case .Interior_Product;   		{ print( "INTERIOR\n" ); }
			case .Left_Complement;    		{ print( "LEFT COMPLEMENT\n" ); }
			case .Right_Complement;   		{ print( "RIGHT COMPLEMENT\n" ); }
		}
	}
	print( "\n" );

	return;
}


#scope_file

is_remaining_input :: inline ( lexer: *Lexer ) -> ( remaining_characters: int ) {
	return lexer.input.count - lexer.input_cursor;
}

peek_next_character :: inline ( lexer: *Lexer ) -> (success: bool, c: u8) {
	
	if !is_remaining_input( lexer ) { 
		return false, 0;	
	}
	else {
		return true, lexer.input[lexer.input_cursor];
	}
}

consume_character :: inline ( lexer: *Lexer ) {

	print( "(Line,Character) == (%,%)\n", lexer.current_line_number, lexer.current_character_index );
	
	if lexer.input[lexer.input_cursor] == #char "\n" {
		print( "This is a newline\n" );
		lexer.current_line_number += 1;	
	}

	lexer.input_cursor 			  += 1;
	lexer.current_character_index += 1;
}

begins_identifier :: inline ( c: u8 ) -> ( yes_no: bool ) {
	return is_alpha( c ) ||  c == #char "_";
}

compose_identifier_keyword_or_basis_symbol_token :: ( lexer: *Lexer, basis_one_element_symbols: [] string ) -> ( token: Token ) {

	continues_identifier :: ( c: u8 ) -> ( yes_no: bool ) {
		return is_alnum( c ) || c == #char "_";
	}

	is_identifier_keyword :: ( ident: string ) -> ( type: Token_Type ) {
	
		type: Token_Type;
		if ident == {
			case "get";		  { type = .Keyword_Get;	   }
			case "set";		  { type = .Keyword_Set;	   }
			case "exit";      { type = .Keyword_Exit; 	   }
			case "basis";  	  { type = .Keyword_Basis;     }
			case "dimension"; { type = .Keyword_Dimension; }
			case;             { type = .Undefined; }
		}

		return type;
	}

	is_identifier_basis_element :: ( ident: string, basis_one_element_symbols: [] string ) -> ( basis_element_index: int ) {
		
		basis_element_index: int = -1;
		for i: 0..basis_one_element_symbols.count-1 {
			if ident == basis_one_element_symbols[i] {
				basis_element_index = i; 
				break;
			}
		}

		return basis_element_index;
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy identifier into token once processed
	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0; 

	while true {
		if t.count > Lexer.TEMP_BUFFER_LENGTH {
			// consume rest of the identifier???
			break;
		}

		success, c = peek_next_character( lexer );
		if success && continues_identifier( c ) {
			t.data[t.count] = c; // .data to ignore array bounds checking
			t.count += 1;
			consume_character( lexer );
		}
		else { break; }
	}

	assert( t.count <= Lexer.TEMP_BUFFER_LENGTH );

	// check if the identifier is a keyword or a basis element first otherwise it is an identifier

	token: Token;
	token.type = .Undefined; 

	keyword_token_type := is_identifier_keyword( cast(string) t );
	if keyword_token_type {
		token.type = keyword_token_type;
		return token;
	}

	basis_element_index := is_identifier_basis_element( cast(string) t, basis_one_element_symbols );
	if basis_element_index >= 0 {
		token.type  = .Basis_Element;
		token.basis = (1 << basis_element_index);
		return token;
	}

	token.type = .Identifier;
	token.identifier.data = talloc( t.count * size_of(u8) );
	memcpy( token.identifier.data, t.data, t.count );
	token.identifier.count = t.count;

	return token;
}

begins_number :: ( c: u8 ) -> ( yes_no: bool ) {
	return is_digit( c );
}

compose_number_token :: ( lexer: *Lexer ) -> ( token: Token ) {

	// This procedure can get very complicated trying to handle floats, hex, overflow, etc...
	// As a first pass just handle integers.
	// Convert the char representation of the numbers to integers as base 10.

	continues_number :: inline ( c: u8 ) -> ( yes_no: bool ) {
		// allow for 1000 and 1_000
		// we parse identifiers before numbers so input like _123 would get handled as an identifier
		return is_digit( c ) || c == #char "_";
	}

	power :: ( a: int, b: int ) -> (c: int) {

		c: int = 1;
		for i: 0..b-1 { c *= a;	}

		return c;
	}

	success: bool;
	c: u8;

	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	while true {
		if t.count > Lexer.TEMP_BUFFER_LENGTH {
			// consume the rest of the digits?
			break;
		}

		success, c = peek_next_character( lexer );
		if success && continues_number( c ) {
			t.data[t.count] = c - #char "0";
			t.count += 1;
			consume_character( lexer );
		}
		else {
			break;
		}
	}

	assert( t.count <= Lexer.TEMP_BUFFER_LENGTH );

	// Pushed all the digits onto the temp buffer (converting from ascii to their integer values).
	// Walk the temp buffer and digits and build final number.
	// Ex. 123 --> 1*(10^2) + 2*(10^1) + 1*(10^0)

	base:  int = 10;
	exponent: int = t.count-1;
	accumulator: int = 0;
	for i: 0..t.count-1 {
		accumulator += (t.data[i] * power(base, exponent));
		exponent -= 1;
	}

	token: Token;
	token.type   = .Number;
	token.number = accumulator;

	return token;
}

compose_separator_or_operator_token :: ( type: Token_Type ) -> ( token: Token ) {

	token: Token;
	token.type = type;

	return token;
}

compose_end_of_input_token :: () -> ( token: Token ) {
	
	token: Token;
	token.type = .End_Of_Input;
	return token;
}

add_token :: ( lexer: *Lexer, token: Token ) {

	assert( lexer.num_tokens < Lexer.MAX_NUM_TOKENS_PER_EXPRESSION );
	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;

	return;	
}
