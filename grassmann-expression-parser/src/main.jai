#scope_export

PROMPT :: ">> ";

/*
	IDENT --> store the nodes from the RIGHT of the .ASSIGN statement

	FLOW OF CONTROL:
		Dimension=4; // if this command is found then we need to prompt for the basis elements
					 // or something haven't figured out the exact mechanics yet
*/

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
		print( "%", PROMPT );
	
		user_input = read_line();
		if user_input == "exit" { break; }

		lexer_set_input( *lexer, user_input );
		lexer_set_basis_one_element_symbols( *lexer, env.one_element_symbols );
		lexer_generate_tokens( *lexer );

		// what do we do if the input is empty? we should skip it, not sure that is happening in the lexer?
		// if no_tokens( *lexer ) { continue; } or something like that

		print( "\nTOKENS:\n" );
		print_tokens( *lexer );

		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser );

		print( "\nAST:\n" );
		print_ast( parser.ast );

		ast    := expand( parser.ast );
		print( "\nPOST-EXPAND AST:\n" );
		print_ast( ast );
		print( "\n" );

		evaluate( ast, *env, *interp );
	/*
		print( "POST EVAL AST:\n" );
		print_ast( result );
		print( "\n" );
	*/

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
#load "queue.jai";
#load "stack.jai";

#import "Basic";
#import "Windows";
#import "File";
