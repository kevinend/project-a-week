#scope_export

PROMPT :: "> ";

main :: () {

	env: 	Environment;
	lexer:  Lexer;
	parser: Parser;
	interp: Interpreter;

	environment_set_dimension_and_n_element( *env, 4 );
	environment_set_one_element_symbols( *env, string.["e1","e2","e3","e4"] );
	environment_generate_basis_elements( *env );

	user_input: string;
	while true {
		
		reset_temporary_storage();

		print( "%", PROMPT );
	
		user_input = read_line();
		if user_input == "exit\r\n" { break; } // replace this with an exit keyword

		lexer_set_input_from_string( *lexer, user_input );
		lexer_generate_tokens( *lexer, env.one_element_symbols );

		if lexer_contains_no_tokens( *lexer ) { 
		    print( "No input; Enter a valid Grassmann expression\n" );
			continue; 
		}

		print_tokens( *lexer, "Tokens:" );

		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser, *interp.symbol_table );

		print_ast( parser.ast, "Ast:" );

		if parser.ast.kind == .Command {
			evaluate_command();
		}
		else {
			parser.ast = expand( parser.ast );
			print_ast( parser.ast, "Post-Expand Ast:" );
			evaluate( parser.ast, *env, *interp );
			print_symbol_table( *interp.symbol_table, env.basis_elements );
		}
	
	}

	return;
}

#scope_file

#load "environment.jai";
#load "file.jai";
#load "token.jai";
#load "lexer.jai";
#load "parser.jai";
#load "ast.jai";
#load "expand.jai";
#load "eval.jai";
#load "stack.jai";

#import "Basic";
#import "Windows";
#import "File";
