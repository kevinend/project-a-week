#scope_export

// what will happen is you get a list of thingies
// set basis = [e1,e2,e3,e4];
// this gets parsed into a command
// that command has a list of arguments, which are their own literal nodes
//  so we get a count of the args (that is the implicit dimension)
//  and we get the length of the first argument
//  allocate space for the one elements
//  copy each argument into that buffer
//  then generate the basis elements
//  update the basis element buffer
//  and point that to the one element symbols

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
		lexer_generate_tokens( *lexer, env.one_element_symbols );

		if lexer_contains_no_tokens( *lexer ) { 
		    print( "No input; Enter a valid Grassmann expression or 'exit;' to quit\n" );
			continue; 
		}

		print_tokens( *lexer, "Tokens:" );

		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser, *interp.symbol_table );

		print_ast( parser.ast, "Ast:" );

		if parser.ast.kind == .Command {
			evaluate_command( parser.ast, *env );
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
