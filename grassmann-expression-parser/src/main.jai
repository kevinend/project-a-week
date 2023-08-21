#scope_export

// i'd like more of a pipeline, boxes and arrows
// [get-input] --> [lex] --> [parse] --> [eval]

// eval needs to know about the nodes in the ast
// parse needs to know about the tokens 
// lex needs the user input
// main needs to read the user input
// file.jai has the read line

// main --> #load file.jai ; #load eval.jai ; #load space.jai
// eval needs the #load space.jai as well
// i don't really know a good way to split this up honestly 

// the problem is that the basis elements and dimension can change dynamically
// maybe you have to make them all modules? I honestly don't know
// 
// i'd like to load all the files but there are dependencies among them

// program crashes on 'exit' without semicolon
// but doesn't exit when entered with the semicolon 'exit;'
// need to look at the parser when the semicolon is missing

// eval should load: lexer.jai and parser.jai, input is just a string and it needs the space parameter
// whch we have in a separate file but we could put in eval
// so we just read the user input then we evaluate it
// then eval goes and does the lexing
// but the parser needs the tokens from the lexer
// if space.is_uninitialized { 
//   basic is different it is all independent things that work together
// }
// there are dependencies in here, so maybe we 
// or we just smash everything together and just call eval anyways?

// file.jai --> read_line();
// eval( user_input, space );
//   then eval just does the generate_tokens
//   but then we have a global lexer and a global parser...hmmm..
//   

main :: () {

	space: Grassmann_Space;
	set_grassmann_space( *space, N = 4, basis_one_elements = string.["e1","e2","e3","e4"] );

	lexer:  Lexer;
	parser: Parser;
	// interp: Interpreter;
	user_input: string;
	while true {
		print( ">> " );
	
		user_input = read_line();
		if user_input == "exit" { break; }

		set_input_from_string( *lexer, user_input );
		set_basis_one_element_symbols( *lexer, space.basis_one_elements );

		// that's the other thing should these all be top-level functions?
		// or should that call into the 
		// that might be my problem as well, all the files are trying to play nice
		// in main, hmm...maybe that is the right approach

		// parse
		// evaluate

		// what do we do if the input is empty? we should skip it, not sure that is happening in the lexer?
		// if no_tokens( *lexer ) { continue; } or something like that
		generate_tokens( *lexer );

		update_parser( *parser, lexer.tokens, lexer.num_tokens );
		generate_abstract_expression_tree( *parser );
	
		print( "\nTOKENS:\n" );
		print_tokens( *lexer );
		
		print( "\nAST:\n" );
		print_ast( parser.ast );
		print( "\n" );
	}

	return;
}

#scope_file

#load "file.jai";
#load "space.jai";
#load "lexer.jai";
#load "parser.jai";

#import "Basic";
#import "Windows";

// i'd like to be able to compile the lexer without any issues...
// hmm...maybe just make it top-level then the lexer can see it?
// or do we pass it in through the lexer
// reference that list the whole time through?
