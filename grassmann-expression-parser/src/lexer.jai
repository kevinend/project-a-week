#scope_export

// TODO: Never implmented the character and line number tracking on the tokens

MAX_NUM_TOKENS_PER_EXPRESSION :: 100;  // an expression is a single line of input
LEXER_TEMP_BUFFER_LENGTH      ::  64; // dictates the size of identifiers and the number of digits in a number
Lexer :: struct {

	current_line_number: 	 Line_Number;
	current_character_index: Character_Index;

	input: string;
	input_cursor: int;

	tokens: [MAX_NUM_TOKENS_PER_EXPRESSION] Token;
	num_tokens: int = 0;

	temp_buffer: [LEXER_TEMP_BUFFER_LENGTH] u8;
}


lexer_set_input :: ( lexer: *Lexer, user_input: string ) {

	lexer.current_line_number 	  = 1;
	lexer.current_character_index = 0;

	lexer.input = user_input;
	lexer.input_cursor = 0;

	return;
}

lexer_generate_tokens :: ( lexer: *Lexer, env: *Environment ) {

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
		while success && is_space( c0 ) {
			consume_character( lexer );
			success, c0 = peek_next_character( lexer );
		}

		if !is_remaining_input( lexer ) {
			token = compose_end_of_input_token();
			add_token( lexer, token );
			break;
		}

		if begins_identifier( c0 ) {
			token = compose_identifier_keyword_or_basis_symbol_token( lexer, env.one_element_symbols );
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
			case .Keyword_Dimension;      	{ print( "KEYWORD_DIMENSION\n" ); }
			case .Keyword_Basis_Elements; 	{ print( "KEYWORD_BASIS_ELEMENTS\n" ); }
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
	lexer.input_cursor += 1;	
}

begins_identifier :: inline ( c: u8 ) -> ( yes_no: bool ) {
	return is_alpha( c ) ||  c == #char "_";
}

compose_identifier_keyword_or_basis_symbol_token :: ( lexer: *Lexer, basis_one_element_symbols: [] string ) -> ( token: Token ) {

	continues_identifier :: ( c: u8 ) -> ( yes_no: bool ) {
		return is_alnum( c ) || c == #char "_";
	}

	is_identifier_keyword :: ( ident: string ) -> ( yes_no: bool, keyword_type: Token_Type ) {
		
		if ident == "Dimension" {
			return true, .Keyword_Dimension;
		}
		else
		if ident == "Basis_Elements" {
			return true, .Keyword_Basis_Elements;
		}
		else 
		if ident == "Help" {
			return true, .Keyword_Help;
		}
		else {
			return false, .Error;
		}
	}

	is_identifier_basis_element :: ( ident: string, basis_one_element_symbols: [] string ) -> ( yes_no: bool, basis_element_index: int ) {
		
		found: bool = false;
		basis_element_index: int = -1;
		for i: 0..basis_one_element_symbols.count-1 {
			if ident == basis_one_element_symbols[i] {
				found = true;
				basis_element_index = i; 
				break;
			}
		}

		return found, basis_element_index;
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy identifier into token once processed
	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0; 

	while true {
		if t.count > LEXER_TEMP_BUFFER_LENGTH {
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

	assert( t.count <= LEXER_TEMP_BUFFER_LENGTH );

	token: Token;
	token.type 		 = .Identifier;
	token.identifier = copy_string( cast(string) t ); // maybe do this at the end if it is truly an identifier??? we copy it and throw it away in the case of keywords and basis elements atm.

	is_keyword, keyword_type := is_identifier_keyword( token.identifier );
	if is_keyword {
		token.type = keyword_type;
	}
	else {
		is_basis_element, index := is_identifier_basis_element( token.identifier, basis_one_element_symbols ); // lexer.basis_one_element_symbols );
		if is_basis_element {
			token.type  = .Basis_Element;
			token.basis = (1 << index); // see note above, wasting the string copy, we don't need the sybmol/identifier anymore
			// maybe put this as the first check then do the keyword then default to the identifier??? hmm..
		}
	}

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
		if t.count > LEXER_TEMP_BUFFER_LENGTH {
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

	assert( t.count <= LEXER_TEMP_BUFFER_LENGTH );

	// pushed all the digits onto the temp buffer (converting from ascii to their integer values).
	// walk the temp buffer and digits and build final number
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

	assert( lexer.num_tokens < MAX_NUM_TOKENS_PER_EXPRESSION );
	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;

	return;	
}
