#scope_export

Lexer :: struct {

	// error fields are publicly accessible from outside this file
	error_reported: bool = false;
	error_message:  string; 

	MAX_NUM_TOKENS_PER_EXPRESSION :: 100;  // an expression is a single line of input
	TEMP_BUFFER_LENGTH      	  ::  64; // dictates the size of identifiers and the number of digits in a number

	current_line_number: 	 Line_Number      = 0;
	current_character_index: Character_Index  = 0;

	// starting position information captured for the current token only
	token_starting_line_number:     Line_Number;
	token_starting_character_index: Character_Index;

	// references to external data: input and basis symbols
	input: string;
	input_cursor: int;

	basis_one_element_symbols: [] string;

	tokens: [MAX_NUM_TOKENS_PER_EXPRESSION] Token;
	num_tokens: int = 0;

	temp_buffer: [TEMP_BUFFER_LENGTH] u8;
}

generate_tokens :: ( lexer: *Lexer, user_input: string, basis_one_element_symbols: [] string ) {

	reset_lexer( lexer, user_input, basis_one_element_symbols );

	// generate tokens based on the current user input.
	c0: u8;
	success: bool;
	token: Token;
	
	while true {

		if lexer.error_reported { break; }
	 	
	 	success, c0 = peek_next_character( lexer );
		while success && is_space( c0 ) { // is_space is from JAI library; Basic???
			consume_character( lexer );
			success, c0 = peek_next_character( lexer );
		}

		mark_start_of_token( lexer );

		if !is_remaining_input( lexer ) {
			token = compose_end_of_input_token( lexer );
			add_token( lexer, token );
			break;
		}

		if begins_literal( c0 ) {
			token = compose_literal_token( lexer );
			add_token( lexer, token );
			continue;
		}

		if begins_identifier( c0 ) {
			token = compose_identifier_keyword_or_basis_symbol_token( lexer, basis_one_element_symbols );
			add_token( lexer, token );
			continue;	
		}

		if begins_number( c0 ) {
			token = compose_number_token( lexer );
			add_token( lexer, token );
			continue;
		}

		token = compose_separator_or_operator_token( c0, lexer );
		add_token( lexer, token );
		if token.type == .Assign {
			swap_tokens( lexer );	
		}
	}

	return;	
}

no_tokens :: ( lexer: *Lexer ) -> ( bool ) {
	// empty input gets the .End_Of_input token
	return lexer.num_tokens <= 1; 
}

print_tokens :: ( lexer: *Lexer, header: string ) {
	
	print( "%\n", header );

	for i: 0..lexer.num_tokens-1 {
		token := lexer.tokens[i];
		if token.type == {
			case .Error;              		{ print( "ERROR" ); }
			case .End_Of_Input;       		{ print( "END_OF_INPUT" ); }
			case .Identifier; 		  	  	{ print( "Identifier == %", token.identifier ); }
			case .Basis_Element;          	{ print( "Basis elements %", token.basis ); }
			case .Literal;					{ print( "Literal %", token.text ); }
			case .Number;     		  		{ print( "Number == %", token.number ); }

			case .Keyword_Get;				{ print( "KEYWORD_GET" ); }
			case .Keyword_Set;				{ print( "KEYWORD_SET" ); }
			case .Keyword_Exit;				{ print( "KEYWORD_EXIT" ); }
			case .Keyword_Dimension;      	{ print( "KEYWORD_DIMENSION" ); }
			case .Keyword_Basis; 			{ print( "KEYWORD_BASIS" ); }

			case .Semicolon; 		  		{ print( "SEMICOLON" );  }
			case .Open_Paren;         		{ print( "OPEN_PAREN" ); }
			case .Close_Paren; 		  		{ print( "CLOSE_PAREN" );}
			case .Open_Square_Brace;  	  	{ print( "OPEN_SQUARE_BRACE" ); }
			case .Close_Square_Brace; 	  	{ print( "CLOSE_SQUARE_BRACE" ); }
			case .Comma;			  	  	{ print( "COMMA" ); }

			case .Plus; 		 	  		{ print( "PLUS"  ); }
			case .Minus;      		  		{ print( "MINUS" ); }
			case .Assign;             		{ print( "ASSIGN" ); }
			case .Exterior_Product;   		{ print( "EXTERIOR" ); }
			case .Regressive_Product; 		{ print( "REGRESSIVE" ); }
			case .Interior_Product;   		{ print( "INTERIOR" ); }
			case .Left_Complement;    		{ print( "LEFT COMPLEMENT" ); }
			case .Right_Complement;   		{ print( "RIGHT COMPLEMENT" ); }
		}
		print( "(Line = %, Start = %, End = %\n", token.line_number, token.character_index.start, token.character_index.end );
	}

	print( "\n" );

	return;
}

#scope_file

reset_lexer :: ( lexer: *Lexer, user_input: string, basis_one_element_symbols: [] string ) {

	// initialize lexer with the input from the user.
	lexer.input        = user_input;
	lexer.input_cursor = 0;

	lexer.current_character_index = 0;

	// capture the current working set of basis one element symbols to use for discerning these symbols from identifiers
	lexer.basis_one_element_symbols = basis_one_element_symbols;

	// clear the tokens from the prior input stream.
	lexer.num_tokens = 0;

	// reset error reported flag
	lexer.error_reported = false;
}
	
report_error :: ( lexer: *Lexer, message: string ) {

	// Lexer errors are for unrecognized character sequences that we can't turn into tokens.
	// Error messages are written to temporary storage, will be wiped on each iteration of the program.
	default_message := tprint( "Lexing error occurred on line number %, character %\n", lexer.token_starting_line_number, lexer.token_starting_character_index );

	error_message: string;
	error_message.count = default_message.count + message.count;
	error_message.data  = talloc( error_message.count );
	assert( error_message.data != null );

	memcpy( error_message.data, default_message.data, default_message.count );
	memcpy( error_message.data + default_message.count, message.data, message.count );

	lexer.error_reported = true;
	lexer.error_message  = error_message;
}

mark_start_of_token :: ( lexer: *Lexer ) {
	
	lexer.token_starting_line_number 	 = lexer.current_line_number;
	lexer.token_starting_character_index = lexer.current_character_index;
}

update_token_character_indices_and_line_number :: inline ( lexer: *Lexer, token: *Token ) {

	token.line_number           = lexer.token_starting_line_number;

	token.character_index.start = lexer.token_starting_character_index;; 
	token.character_index.end   = lexer.current_character_index-1;
}

is_remaining_input :: inline ( lexer: *Lexer ) -> ( bool ) {
	return (lexer.input.count - lexer.input_cursor) > 0;
}

peek_next_character :: inline ( lexer: *Lexer ) -> (success: bool, c: u8) {
	
	if is_remaining_input( lexer ) { 
		return true, lexer.input[lexer.input_cursor];
	}
	else {
		return false, 0;
	}
}

consume_character :: inline ( lexer: *Lexer ) {

	if lexer.input[lexer.input_cursor] == #char "\n" {
		lexer.current_line_number += 1;	
	}

	lexer.input_cursor 			  += 1;
	lexer.current_character_index += 1;
}

begins_literal :: inline ( c: u8 ) -> ( yes_no: bool ) {
	return c == #char "'";
}

error_token :: () -> ( token: Token ) {
	
	token: Token;
	token.type = .Error;

	return token;
}

compose_literal_token :: ( lexer: *Lexer ) -> ( token: Token ) {

	continues_literal :: ( c: u8 ) -> ( yes_no: bool ) {
		return c != #char "'";
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy literal (excluding quotes) into token
	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	consume_character( lexer ); // initial single quote
	while true {
		if t.count > Lexer.TEMP_BUFFER_LENGTH {
			report_error( lexer, "The current literal exceeds the maximum allowed length.\n" );
			return error_token();
		}

		success, c = peek_next_character( lexer );
		if success && continues_literal( c ) {
			t.data[t.count] = c;
			t.count += 1;
			consume_character( lexer );
		}
		else {
			break;
		}
	}

	if c == #char "'" {
		consume_character( lexer );
	}
	else {
		report_error( lexer, "Ending quote from literal is missing\n" );
		return error_token();
	}

	token: Token;
	token.type = .Literal;
	token.text.data = talloc( t.count * size_of( u8 ) );
	memcpy( token.text.data, t.data, t.count );
	token.text.count = t.count;

	update_token_character_indices_and_line_number( lexer, *token );

	return token;
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
			report_error( lexer, "Identifer exceeded maximum length" );
			return error_token();
		}

		success, c = peek_next_character( lexer );
		if success && continues_identifier( c ) {
			t.data[t.count] = c; // .data to ignore array bounds checking
			t.count += 1;
			consume_character( lexer );
		}
		else { break; }
	}

	// check if the identifier is a keyword or a basis element first otherwise it is an identifier

	token: Token;
	token.type = .Undefined; 

	keyword_token_type := is_identifier_keyword( cast(string) t );
	if keyword_token_type {
		token.type = keyword_token_type;
		update_token_character_indices_and_line_number( lexer, *token );
		return token;
	}

	basis_element_index := is_identifier_basis_element( cast(string) t, basis_one_element_symbols );
	if basis_element_index >= 0 {
		token.type  = .Basis_Element;
		token.basis = (1 << basis_element_index);
		update_token_character_indices_and_line_number( lexer, *token );
		return token;
	}

	token.type = .Identifier;
	token.identifier.data = talloc( t.count * size_of(u8) );
	memcpy( token.identifier.data, t.data, t.count );
	token.identifier.count = t.count;

	update_token_character_indices_and_line_number( lexer, *token );

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
			report_error( lexer, "Maximum number of digits exceeded\n" );
			return error_token();
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

	update_token_character_indices_and_line_number( lexer, *token );

	return token;
}

compose_separator_or_operator_token :: ( c0: u8, lexer: *Lexer ) -> ( token: Token ) {

	token: Token;
	if c0 == {
		// separators
		case #char ";"; { 
			consume_character( lexer );
			token.type = .Semicolon;
		}
		case #char "("; {
			consume_character( lexer );
			token.type = .Open_Paren;
		}
		case #char ")"; {
			consume_character( lexer );
			token.type = .Close_Paren;
		}
		case #char "["; {
			consume_character( lexer );
			token.type = .Open_Square_Brace;
		}
		case #char "]"; {
			consume_character( lexer );
			token.type = .Close_Square_Brace;
		}
		case #char ","; {
			consume_character( lexer );
			token.type = .Comma;
		}

		// operators
		case #char "+"; { 
			consume_character( lexer );
			token.type = .Plus;
		}
		case #char "-"; {
			consume_character( lexer );
			token.type = .Minus;
		}
		case #char "="; {
			consume_character( lexer );
			token.type = .Assign;
		}
		case #char "^"; { 
			consume_character( lexer );
			token.type = .Exterior_Product;
		}
		case #char "!"; {
			// !^ is the regressive product (as in 'not the exterior product)
			consume_character( lexer );
			
			success, c1 := peek_next_character( lexer );
				
			if success && c1 == #char "^" {
				consume_character( lexer );
				token.type = .Regressive_Product;
			}
			else {
				report_error( lexer, "Unrecognized character sequence, expected symbol ^ after symbol !\n" );
				return error_token();

			}
		}
		case #char "@"; {
			consume_character( lexer );
			token.type = .Interior_Product;
		}
		case #char "<"; {
			// <<  is the left complement for now
			consume_character( lexer );

			success, c1 := peek_next_character( lexer );

			if success && c1 == #char "<" {
				consume_character( lexer );
				token.type = .Left_Complement;
			}
			else {
				report_error( lexer, "Unrecognized character sequence, expected symbol < after symbol <\n" );
				return error_token();
			}
		}
		case #char ">"; {
			// >> is the right complement for now
			consume_character( lexer );
			
			success, c1 := peek_next_character( lexer );

			if success && c1 == #char ">" {
				consume_character( lexer );
				token.type = .Right_Complement;
			}
			else {
				report_error( lexer, "Unrecognized character sequence, expected symbol > after symbol >\n" );
				return error_token();
			}
		}
		case; {
			report_error( lexer, "Unrecognized operator or separator." );
			return error_token();
		}
	}

	update_token_character_indices_and_line_number( lexer, *token );
	return token;
}

compose_separator_or_operator_token :: ( lexer: *Lexer, type: Token_Type ) -> ( token: Token ) {

	token: Token;
	token.type = type;

	update_token_character_indices_and_line_number( lexer, *token );

	return token;
}

compose_end_of_input_token :: ( lexer: *Lexer ) -> ( token: Token ) {

	token: Token;
	token.type = .End_Of_Input;

	// since we don't consume a character the ending index is before the start
	// i don't think this is a problem but putting a note here in case it is!
	update_token_character_indices_and_line_number( lexer, *token );

	return token;
}

add_token :: ( lexer: *Lexer, token: Token ) {

	assert( lexer.num_tokens < Lexer.MAX_NUM_TOKENS_PER_EXPRESSION );
	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;

	return;	
}

swap_tokens :: ( lexer: *Lexer ) {

	// This is exclusively to handle assignment statements.
	// The order of 'Identifier' followed by 'Assignment' is annoying in the parser so I just swap the tokens in the lexer.

	c: int = lexer.num_tokens-2; 
	p: int = lexer.num_tokens-1;

	previous: Token = lexer.tokens[p];
	current:  Token = lexer.tokens[c];
	
	temp: Token = previous;
	lexer.tokens[p] = current;
	lexer.tokens[c] = temp;
}
