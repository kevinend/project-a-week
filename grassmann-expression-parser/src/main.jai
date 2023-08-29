#scope_export

PROMPT :: ">> ";

// These are the two approaches.
// Input --> Lex --> Parse --> Expand/Distribute --> Eval (tree walk)
// Input --> Lex --> Parse --> Expand --> Compile --> Eval (stack)
// The symbol table will just store the AST for the identifier
// Then when we get a new expression we just inject that value into there

/*
	IDENT --> store the nodes from the RIGHT of the .ASSIGN statement
*/

main :: () {

	set_grassmann_space( *space, N = 4, basis_one_elements = string.["e1","e2","e3","e4"] );

	lexer:  Lexer;
	parser: Parser;
	// interp: Interpreter;
	user_input: string;
	while true {
		print( "%", PROMPT );
	
		user_input = read_line();
		if user_input == "exit" { break; }

		lexer_set_input( *lexer, user_input );
		lexer_set_basis_one_element_symbols( *lexer, space.basis_one_elements );
		lexer_generate_tokens( *lexer );

		// what do we do if the input is empty? we should skip it, not sure that is happening in the lexer?
		// if no_tokens( *lexer ) { continue; } or something like that
		
		parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
		parser_generate_ast( *parser );
	
		print( "\nTOKENS:\n" );
		print_tokens( *lexer );
		
		print( "\nAST:\n" );
		print_ast( parser.ast );
		print( "\n" );
	}

	return;
}

#scope_file

#load "space.jai";
#load "file.jai";
#load "token.jai";
#load "lexer.jai";
#load "parser.jai";
#load "ast.jai";

#import "Basic";
#import "Windows";
#import "File";
