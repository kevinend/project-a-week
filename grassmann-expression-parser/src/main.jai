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

	environment_update( 
		 *env
		,dimension = 4
		,one_element_symbols = string.["e1","e2","e3","e4"]
	);

	user_input: string;
	while !quit {
		
		reset_temporary_storage();

		print( "%", PROMPT );
	
		user_input = read_line();
		generate_tokens( *lexer, user_input, env.basis_one_element_symbols );
		
		if lexer.error_reported {
			print( lexer.error_message );
			continue;
		}
		else
		if no_tokens( *lexer ) {
			print( "No input; Enter a valid Grassmann expression or 'exit;' to quit\n" );
			continue;
		}
		
		print_tokens( *lexer, "Tokens:" );

		generate_ast( *parser, lexer.tokens, lexer.num_tokens, *interp.symbol_table ); // this should handle expansion automatically
		if syntax_errors( parser.ast ) {
			continue;
		}

		/*
			two ideas from this PDF that stuck out are the parsing of the WHILE Statement
				case Token_While: {
					nextToken();
					check(toklParen);
					Expression condition = parseExpression();
					check(Tokrparen);
					statement body = parseStatement( PSScope, null, &endloc );
					s = new WhileStatement( loc, condition, body, endloc );
					break
				}

				lookahead is a stack of tokens, if we want to lookahead we just stack them up

				Commands
				Expressions

				assignments are binary operators but you can only have one of them...
				Assignments parse_expression
				Expressions
				assignment
				expression
				
				
				next+1
				next
				current

				so that is a cool idea, then you pop them off when you need them to do the thing

				Special ERROR- AST NODE
				replace errorneous AST node with an 'error node'
				replace any node that has an error child with an error node
					removes the cascading errors


				like that looks super simple to me, check
			but right now we have a problem with errors
				if t0.category == .Command {
					root = parser_command( parser );
				}
				else {
					root = parse_statement( parser, symbol_table );

					if !Basis_Element || !Ident || !Number {
						report_error( "Syntax error, unexpected token "
					}
					else
					if t1.Assign and !Ident then we have a problem
					// you have all these potential problems, how do people solve this??
				}

						eval( parser.ast );
				// if this is a command, then we check and do the command
				// if it is an assignment we do the assign
				// otherwise we eval

			print_symbol_table (this could be an option or it could always just be displayed )

		*/

	//	parser_set_tokens( *parser, lexer.tokens, lexer.num_tokens );
	//	parser_generate_ast( *parser, *interp.symbol_table );


		if parser.ast.kind == .Command {
			evaluate_command( parser.ast, *env );
		}
		else {
			parser.ast = expand( parser.ast );
			print_ast( *parser, "Post-Expand Ast:" );
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
#load "error-pass.jai";
#load "ast.jai";
#load "expand.jai";
#load "eval.jai";
#load "stack.jai";

#import "Basic";
#import "Windows";
#import "File";
