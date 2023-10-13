#scope_export


/*
	Application is a command-line (GUI coming) calculator for Grassmann expressions 
	augmented with a symbol table to store intermediate calculations for use in
	later expressions.

	It also handles simple commands to change the current linear space's
	dimension, so moving from G(R3) to G(R4) as an example, or changing
	the basis elements of the space in case you don't like the defaults.

	For explanation of notation and data layouts for the underlying space,
	please see the file environment.jai.

*/

PROMPT :: "> ";
quit: bool = false;
main :: () {

	env: 	Environment;
	lexer:  Lexer;
	parser: Parser;
	interp: Interpreter;

	environment_update( *env, dimension = 4, one_element_symbols = string.["e1","e2","e3","e4"] );

	user_input: string;
	while !quit {
		
		reset_temporary_storage();

		print( "%", PROMPT );
	
		user_input = read_line();

		lexer_set_input_from_string( *lexer, user_input );
		lexer_clear_prior_tokens( *lexer );
		lexer_generate_tokens( *lexer, env.one_element_symbols );

		if lexer_reported_error( *lexer ) {
			lexer_print_error_message( *lexer ); 
			lexer_reset_error_status(  *lexer );
			continue;
		}
		else
		if lexer_contains_no_tokens( *lexer ) { 
		    print( "No input; Enter a valid Grassmann expression or 'exit;' to quit\n" );
			continue; 
		}

		lexer_print_tokens( *lexer, "Tokens:" );

		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser, *interp.symbol_table );

		if parser_reported_error( *parser ) {
			print( parser.error_message );
			parser_reset_error_status( *parser );
			continue;
		}

		parser_print_ast( parser.ast, "Ast:" );

		if parser.ast.kind == .Command {
			evaluate_command( parser.ast, *env );
		}
		else {
			parser.ast = expand( parser.ast );
			parser_print_ast( parser.ast, "Post-Expand Ast:" );
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
