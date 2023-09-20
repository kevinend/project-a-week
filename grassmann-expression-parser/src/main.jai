#scope_export

PROMPT :: ">> ";

main :: () {

	env: 	Environment;
	lexer:  Lexer;
	parser: Parser;
	interp: Interpreter;

	print( "K_Element; Size of == %\n", size_of(K_Element_Node) * 64 * 8);
	print( "Binary_Op; Size of == %\n", size_of(Binary_Operator_Node) * 63 * 8 );

	environment_set_dimension_and_n_element( *env, 4 );
	environment_set_one_element_symbols( *env, string.["e1","e2","e3","e4"] );
	environment_generate_basis_elements( *env );

	user_input: string;
	while true {
		print( "%", PROMPT );
	
		user_input = read_line();
		if user_input == "exit" { break; }

		lexer_set_input( *lexer, user_input );
		lexer_generate_tokens( *lexer, *env );

		print_tokens( *lexer, "Tokens:" );

		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser, *interp.symbol_table );

//		print( "\nAST:\n" );
//		print_ast( parser.ast, "AST:" );

		ast    := expand( parser.ast );
//		print( "\nPOST-EXPAND AST:\n" );
//		print_ast( ast, *env );
//		print( "\n" );

		evaluate( ast, *env, *interp );

		print_symbol_table( *interp, *env );

		reset_temporary_storage();
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
