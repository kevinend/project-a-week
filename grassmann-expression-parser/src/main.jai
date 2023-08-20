#scope_export

main :: () {

	space: Grassmann_Space;
	set_grassmann_space( *space, N = 4, basis_one_elements = string.["e1","e2","e3","e4"] );

	std_in_handle := GetStdHandle( STD_INPUT_HANDLE ); // doesn't check for error
	std_in := File.{ handle = std_in_handle };

	lexer:  Lexer;
	parser: Parser;
	user_input: string;
	while user_input != "exit" {
		
		user_input = read_line( std_in );

		set_input_from_string( *lexer, user_input );
		set_basis_one_element_symbols( *lexer, space.basis_one_elements );

		// what do we do if the input is empty? we should skip it, not sure that is happening in the lexer?
		// if no_tokens( *lexer ) { continue; } or something like that
		generate_tokens( *lexer );

		update_parser( *parser, lexer.tokens );
//		generate_abstract_expression_tree( *parser );

		print_tokens( *lexer );
	}

	return;
}

#scope_file

#load "space.jai";
#load "lexer.jai";
#load "parser.jai";

#import "Basic";
#import "Windows";

// i'd like to be able to compile the lexer without any issues...
// hmm...maybe just make it top-level then the lexer can see it?
// or do we pass it in through the lexer
// reference that list the whole time through?
