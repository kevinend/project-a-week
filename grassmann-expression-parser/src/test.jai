#scope_export


// @Incomplete: Jai doesn't have an abstraction for reading from stdin yet...
/*
        #if OS == .WINDOWS {
            handle := GetStdHandle(STD_INPUT_HANDLE); // GetStdHandl
            bytes_read: u32;
            success := ReadFile(handle, buffer.data + input.count, cast(u32) (buffer.count - input.count), *bytes_read, null);

*/

// Powers of 2
// 2^1 = 2 
// 2^2 = 4
// 2^3 = 8
// 2^4 = 16
// 2^5 = 32
// 2^6 = 64, so if you dimension is six we only handle 64 bits worth of k-vectors
// 

// 2^3 
// 1-scalar
// 3-vectors
// 3-bivectors
// 1-trivector
// that is 8 total, what we going to do about it
// we ideally set the array equal to the max
// 2^3
// when you have e1, maybe we put the scalars at the end?
// Basis: e1 e2 e3
// Index: 0  1  2
// Integer Value: 1 2 4 
// 
// Possible values: scalar, e1, e2, e3, e1e2, e13, e23, e123
//  (1) scalar = 0???
//  (2) e1     = 1
//  (3) e2     = 2
//  (4) e3 	   = 4
//  (5) e1e2   = 3
//  (6) e1e3   = 5
//  (7) e2e3   = 6
//  (8) e1e2e3 = 7

// if we break it off into several different'types then the mapping is into a new type
// you would have the 1-vector
// 2-vector
// 3-vector
// etc...
// which you may or may not want for a representation
// really a k-vector can be distinguished from it's grade
// if we split them up then we don't need to create a new object
// well you would because you would be combining two of them

// Regressive Product -- Intersection/Factorization
// so in the book he says a one element regressive N element gives you a factorization
// of the one element in terms of the N element
//
// 
/*
	e1 regressive e1e2e3 by the common factor theorem
	1 + N = (1+N)-N = 1-element in terms of the second factor

		 e1      -e2       e3
		not(e1) not(-e2) not(e3)
	(e1 not(e1))e1 + (e1 not(-e2))e2 + (e1 not(e3))e3 // this comes out this way, shouldn't it come out this way in general???

	in general a 1 element can be a sum of of other 1 elements so it might be
	(e1+e2+e3) regressive N, which then decomposes into each part of the element, since some will be non-zero

	when John says "one-element" think the most general one element, not a single vector, a sum of 1-vectors

	regressive product is distributive over addition so this turns into something like
	(e1 ^ not(e1))e1 + (e2 ^ not(e2))e2 + (e3 ^ not(e3))e3

	there are a lot of terms in there that are zero, because you have to expand all of them but in the end this is what is returned
	in general there would be 9 terms and 6 of them would be zero

	########################################
	so what if you had to 'factor' a 2-element in a 3-space into a product of 1-elements (this is always the goal)?
	(e1e2 + e1e3 + e2e3) regressive (e1e2 + e1e3 + e2e3) == (2+2)-3 = (4)-3 = 1
	   Grade-2                        Grade-2

	   // like if you said to span this element, how would you do it?
	   // you'd have to distribute over the general two-element and take the regressive product

	   // say you have e1e2 and you say, give me the 1-span of it and the 1-cospan
	   // i would literally take the product with the general two element

	   e1e2 regressive (e1e2 + e1e3 + e2e3)
	   (e1e2 regressive e1e2)  + (e1e2 regressive e1e3) + (e1e2 regressive e2e3)
	    
	    e1e3:
	    1-span:   {e1,e3}
	    1-cospan: {e3,-e1}

		(e1e2 ^e1)e3 + (e1e2^e3)-e1 + (e1e2^e2)e3 + (e1e2^e3)e2
		// it has the terms you are looking for but it doesn't say these are the factors..
		// s1e1 + s2e2

		// we might just need to distribute and do the regressive product with the general element that gives us an
		// N+1 element and express it over that

		// i think it would go to zero if you tried to factor the same thing the same way



	   Grade of M = 2
	   1-span: Grade=1
	   1-copsan: (2-1) = 1

		// i'm thinking about the cospan wrong it is about the M-element, not the N-element
		// so how do you span this element?
		1-span:   {e1
	   	1-cospan: {

	what exactly would this look like?
	(e1e2 ^ not(e1e2))e1e2, this isn't a one element, so i got some of my facts mixed up here...

	// i think you have to distribute and then roll with the punches, i don't know another way to do it

*/

// here are the two options

K_Vector :: struct {
	grade: 			int;
	basis_elements: int; 	// this is a bit-vector (e1,e2,e3, e1
	coefficient:    float;  // some scalar multiple
}
// to get the basis_elements you need to look up the offset in the table and then can get the values
// if they are all k-vectors you can just put them in a list
// if they are separate you can't necessarily do that...hmm
// you don't need the offset lookups either

power :: ( a: int, b: int ) -> (c: int) {

	c: int = 1;
	for i: 0..b-1 {
		c *= a;	
	}

	return c;
}

// n-choose-k
// build the list of offsets

choose :: (n: int, k: int) -> (value: int) {
	// example: choose(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * choose(n-1, k-1)) / k;
}

// so the first step is to generate a list of the 1 elements then smash them
// together, if we end up representing bivectors and stuff differently this may all work out with using the span...
//

Vector2D :: struct { x: int; y: int; }

Vector_Type :: enum {
	 OneD;
	 TwoD;
	 ThreeD;
}

recurse :: ( x: int )  {
	if x == 0 { return; }
	print( "x == %\n", x );
	recurse( x-1 ); // tail recursion, the next instruction is outside of there so you don't see the stack
	print( "Tail Recursion Unwind\n" );
}

// Token is just an anonymous union of the values
// need to know the keywords ahead of time but we don't know the values
/*
	process is
	dimension(4);
	basis_elements(e1,e2,e3,e4);

Loop over this part:
	L1 = (e1^e2) + (e1^e2);
	L2 = (e3)
	L  = L1 ^ L2
	R  = eval(L); // print results and store in R

*/

// so for the keyword we don't need to know anything, the basis elements are NOT key words
// read an identifier
// when we parse we check if it is a basis element, then we return the value
// otherwise we subsitute the value in and parse that I guess
// maybe that flow changes not sure though

Line_Number 	 :: #type, distinct int;
Character_Index  :: #type, distinct int;

Range :: struct(T: Type) {
	start: T;
	end:   T;
}

Token_Type :: enum u8 {

	Error;

	Operator_Plus;
	Operator_Minus;
	Operator_Exterior_Product;
	Operator_Regressive_Product;
	Operator_Interior_Product;
	Operator_Left_Complement;
	Operator_Right_Complement;

	Identifier;
	Number;

	Keyword_Basis_Elements;
	Keyword_Eval;
	Keyword_Exit;
	Keyword_Help;
	Keyword_Dimension;
	Keyword_Print;
}

Token :: struct {
	
	type: Token_Type = .Error;

	line_number: 	 Range(Line_Number);
	character_index: Range(Character_Index);

	union {
		identifier: string;
		number:     float;
	}	
}

// he doesn't have stuff to work with console input...
// jesus christ i'd have to deal with the 
// which kind of sucks dude 
// but i guess we could just set up a ring buffer?
// a buffer that overwrites itself and use his input to grab the keyboard input, i guess?

// his lexer doesn't output the tokens it just puts them in a list and iterates through them
// we want to keep the tokens around but we want a buffer of the tokens
// so we can say there are a maximum number of concurrent tokens of whatever, then we just reset and keep track of the
// number of tokens

// this value is arbitrary but it if you have input like the following:
// L  = (e1^e2) + (e3);
// L1 = L ^ (e4);
// eval(L1);

// do we want to convert L into a tree structure? and have L1 point into it? I am undecided on the details here atm

MAX_NUM_TOKENS_PER_EXPRESSION :: 100; // temp token buffer for the current expression; reset/clear on each subsequent expression
MAX_IDENTIFIER_LENGTH         ::  10;
Lexer :: struct {

	current_line_number: 	 Line_Number;
	current_character_index: Character_Index;

	input: string;
	input_cursor: int;

	tokens: [MAX_NUM_TOKENS_PER_EXPRESSION] Token;
	num_tokens: int = 0;

	temp_identifier_buffer: [MAX_IDENTIFIER_LENGTH] u8;
}

// Honestly this is odd to admit but i don't write too many interactive programs so this is kind of interesting
// not sure the overall structure though this looks a little bit janky atm
// TODO: we still don't have a way to capture standard input in this scheme
// While 1, do the following
//  print splash page/help
//   while input != "exit" {
//		while dimension == -1 || basis_elements.count != dimension {
//			prompt user to enter this information
//			dimension = 4;
//			basis_elements = e1,e2,e3,e4;
//		}
//     read the user's input
//     lex the input into a sequence of tokens
//	   check for keywords like dimension, basis_elements, exit	 
//  }

read_line :: ( file: File ) -> ( user_input: string ) {

	// Read the input from the user through the terminal
	user_input: string;
	user_input.data = user_input_buffer.data;	
	
	while true {
		bytes_read: u32;
		success := ReadFile( file.handle, user_input_buffer.data, cast(u32)USER_INPUT_BUFFER_SIZE, *bytes_read, null );
		if bytes_read {
			user_input.count = bytes_read;
			if user_input.data[user_input.count-1] == #char "\n" { // read a whole line
				user_input.count -= CARRIAGE_RETURN_AND_NEWLINE;
				break; 
			}
			else {
				// Input exceeds the fixed buffer length
				// Ignore the input, flush stdin, and continue to the next iteration
				print( "Expression length exceeded limit of % characters ignoring input\n", MAX_USER_INPUT_LENGTH );
					
				user_input.count = 0;

				// there has to be a better way to flush stdin...
				temp: u8;
				success := ReadFile( file.handle, *temp, 1, *bytes_read, null );
				while temp != #char "\n" { // do I need to check EOF as well???
					success = ReadFile( file.handle, *temp, 1, *bytes_read, null );
				}

				continue;
			}
		}
	}

	return user_input;
}

set_input_from_string :: ( lexer: *Lexer, user_input: string ) {

	lexer.current_line_number 	= 1;
	lexer.current_character_index = 0;

	lexer.input = user_input;
	lexer.input_cursor = 0;

	return;
}

peek_next_character :: inline ( lexer: *Lexer ) -> (success: bool, c: u8) {
	
	if !is_remaining_input( lexer ) { 
		return false, 0;	
	}
	else {
		return true, lexer.input[lexer.input_cursor];
	}
}

consume_character :: inline ( lexer: *Lexer ) {
	lexer.input_cursor += 1;	
}

is_remaining_input :: inline ( lexer: *Lexer ) -> ( remaining_characters: int ) {
	return lexer.input.count - lexer.input_cursor;
}

begins_identifier :: inline ( c: u8 ) -> ( yes_no: bool ) {
	return is_alpha( c ) ||  c == #char "_";
}

compose_identifier_token :: ( lexer: *Lexer ) -> ( token: Token ) {

	continues_identifier :: ( c: u8 ) -> ( yes_no: bool ) {
		return is_alnum( c ) || c == #char "_";
	}

	success: bool;
	c: u8;

	identifier: string;
	identifier.data  = lexer.temp_identifier_buffer.data;
	identifier.count = 0; 

	// add the leading character which can only be an underscore, a-z or A-Z
	success, c = peek_next_character( lexer );
	identifier.data[identifier.count] = c; // .data to ignore array bounds checking
	identifier.count += 1;
	consume_character( lexer );

	while is_remaining_input( lexer ) {
		if identifier.count > MAX_IDENTIFIER_LENGTH {
			// consume all the rest of the identifier???
			break;
		}

		success, c = peek_next_character( lexer );
		if success && continues_identifier( c ) {
			identifier.data[identifier.count] = c; // .data to ignore array bounds checking
			identifier.count += 1;
			consume_character( lexer );
		}
		else { break; }
	}

	assert( identifier.count <= MAX_IDENTIFIER_LENGTH );

	token: Token;
	token.type 		 = .Identifier;
	token.identifier = identifier; // should we string copy this? or add it to a set???

	return token;
}

add_token :: ( lexer: *Lexer, token: Token ) {

	assert( lexer.num_tokens < MAX_NUM_TOKENS_PER_EXPRESSION );
	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;

	return;	
}

generate_tokens :: ( lexer: *Lexer ) {

	c: u8;
	success: bool;
	token: Token;
	while true { 
	 	
	 	success, c = peek_next_character( lexer );
		while success && is_space( c ) {
			consume_character( lexer );
			success, c = peek_next_character( lexer );
		}

		if !is_remaining_input( lexer ) {
			// we should make a token here marking the end of the input
			break;
		}

		if begins_identifier( c ) {
			token = compose_identifier_token( lexer );
			add_token( lexer, token );
		}
	}

	print( "The input cursor value is %\n", lexer.input_cursor );

	return;	
}

// new_identifier_token

MAX_USER_INPUT_LENGTH		:: 8;
CARRIAGE_RETURN_AND_NEWLINE :: 2;
USER_INPUT_BUFFER_SIZE 		:: MAX_USER_INPUT_LENGTH + CARRIAGE_RETURN_AND_NEWLINE;

user_input_buffer: [USER_INPUT_BUFFER_SIZE] u8;

// i still don't understand how to use the symbol table effectively
// i also don't know how to do the string thing he suggested either
// symbol table
//  L: value

main :: () {

	std_in_handle := GetStdHandle( STD_INPUT_HANDLE );
	std_in := File.{ handle = std_in_handle };

	lexer: Lexer;
	user_input: string;
	while user_input != "exit" {

		user_input = read_line( std_in );

		set_input_from_string( *lexer, user_input );
		generate_tokens( *lexer );

		// don't exactly want to parse this whole thing yet.. hmmm...
		// this guy made an interesting remark about strings, instead of comparing strings you just compare pointers
		// as a way to save the comparison
		// just want to check if the 

		// if is_environment_directive( tokens ) { update the environment }
		// else
		// if tokens[0] == "eval" { eval_expression( user_input ) }
		// else 
		// if environment_configs
		//   update the environment
		// else
		// if 
	}
		



/*
	{
		MAX_INPUT_LENGTH 			:: 8;
		CARRIAGE_RETURN_AND_NEWLINE :: 2;
		BUFFER_SIZE 				:: MAX_INPUT_LENGTH + CARRIAGE_RETURN_AND_NEWLINE;

		buffer: [BUFFER_SIZE] u8; 
		input: string;
		input.data = buffer.data;

		handle := GetStdHandle( STD_INPUT_HANDLE );
		file := File.{ handle = handle };
		while true {
			bytes_read: u32;
			success := ReadFile( handle, buffer.data, cast(u32)(BUFFER_SIZE), *bytes_read, null );
			if bytes_read {
				input.count = bytes_read;
				if input.data[input.count-1] == #char "\n" {
					// valid input
					print( "Valid input\n" );	
				}
				else {
					// Input exceeds the fixed buffer length
					// Ignore the input, flush stdin, and continue to next iteration

					print( "Expression length exceeded limit of % characters ignoring input\n", MAX_INPUT_LENGTH );
					
					input.count = 0;

					// there has to be a better way to flush stdin...
					temp: u8;
					success := ReadFile( handle, *temp, 1, *bytes_read, null );
					while temp != #char "\n" { // do I need to check EOF as well???
						success = ReadFile( handle, *temp, 1, *bytes_read, null );
					}

					continue;
				}
			}

			if input == "exit" { break; }
			else 			   { print( "Input == %\n", input ); } 
		}

		// so this is our 'get_user_input_from_console()'
	}
*/
	
	{
		values: [5] int;
		print( "Values.count == %\n", values.count );
	}

	// all i want to do is figure out how to debug an array using remedybg
	// so it can't figure out this array type, i wonder if we just put a size on the data field?
	// yep, that worked, thank you Casey for that
	// what if the data type is more complex like a simple vector2D or something?
	// okay i had to move the type to the top-level, can't handle the type in the sub-scope, whatever no biggie

	recurse( 3 ); // so you can lock a value, you can use {*} to find it in any location
	// that is cool to see what is loaded in the gamestate or something like why the eff don't i see the assets
	// you can also change the memory format specifier, now I can actually look at token values
	// what if we have an array of pointers?

	// so i think part of it was that i was getting flustered and not thinking clearly about what i wanted to 
	// actually do. Just slow down, nothing has to be perfect but just give it your best shot every day and you
	// will make it. I promise!!!

	// some more testing with the features that i want	
	

	r: Range(int);
	r.start = 0;
	r.end   = 10;

	{
		values: [5] int;
		for i: 0..4 {
			values[i] = i+1;
		}
		print( "Values == %\n", values );
	}

	{
		values: [..] int;
		for i: 0..4 {
			array_add(*values, (i+1) );
		}
		print( "Values == %\n", values );
	}

	{
		values: [..] Vector2D;
		for i: 0..4 {
			v := Vector2D.{ x = (i+1), y = (i+1)*10 };
			array_add(*values, v );
		}
		print( "Values == %\n", values );
	}

	{
		values: [..] *Vector2D;
		for i: 0..4 {
			v := New(Vector2D);
			v.x = (i+1);
			v.y = (i+1) * 10;
			array_add( *values, v );
		}
		print( "Values == %\n", values );
	}

	{
		values: [..] Vector_Type;
		v1 := Vector_Type.OneD;
		v2 := Vector_Type.TwoD;
		v3 := Vector_Type.ThreeD;

		array_add( *values, v1 );
		array_add( *values, v2 );
		array_add( *values, v3 );

		print( "Values == %\n", values );

	}

	Test :: enum { 
		One;
		Two;
	}

	Type1 :: struct {
		test: Test;
		s: string;
	}

	print( "Size of (string) == %\n", size_of(string) );
	print( "Size of (type1)  == %\n", size_of(Type1) );

	{
		Vector3  :: struct { x,y,z: float; }

		NewType1 :: struct {
			#as using _v: Vector3;
			value1: int;
		}

		NewType2 :: struct {
			#as using v: Vector3;
			value2: float;
		}

		take_vector3 :: ( v: Vector3 ) {
			print( "(x,y,z)==(%,%,%)\n", v.x, v.y, v.z );
			return;
		}

		nt1 := NewType1.{ x = 1,  y = 2,  z = 3,  value1 = 4 };
		nt2 := NewType2.{ x = 10, y = 20, z = 30, value2 = 40 };
	
		// now that is a cool feature!
		take_vector3( nt1 );
		take_vector3( nt2 );
	}

	{
		Type_Variant :: enum {
			Type1;
			Type2;
			Type3;
		}
		
		Base :: struct {
			type: Type_Variant;
		}

		Type1 :: struct {
			#as using _type: Base;
			field1: int;
		}

		Type2 :: struct {
			#as using _type: Base;
			field2: float;
		}

		// cool so the subtype works on the import of the data
		take_base :: ( node: *Base ) {

			if node.type == Type_Variant.Type1 {
				print( "Got a type1\n" );
			}
			else
			if node.type == Type_Variant.Type2 {
				print( "Got a type2\n" );	
			}

			return;
		}

		t1 := New(Type1);
		t1.type = .Type1;

		t2 := New(Type2);
		t2.type = .Type2;

		take_base( t1 );
		take_base( t2 );

		give_base :: ( variant: Type_Variant ) -> ( node: *Base ) {
	
			if variant == .Type1 {
				t1 := New(Type1);
				t1.type = .Type1;
				return t1;
			}
			else
			if variant == .Type2 {
				t2 := New(Type2);
				t2.type = .Type2;
				return t2;
			}
		}

		t11: *Type1 = cast(*Type1)give_base( .Type1 );
		t22: *Type2 = cast(*Type2)give_base( .Type2 );
	}

	{
		// so how does this benefit us in what we are trying to do???
		// we can't pass an Expression_Node to some functions without a ton of casting
		// i don't even know if this works on the return
	}

	some_float: float = 3.14;
	some_float_string: string = tprint( "%", some_float );
	print( "some_float_string == %\n", some_float_string ); // well that worked

	output_basename := "kevin";
	output_filename := tprint("%.lib", output_basename);
	print( "Output filename == %\n", output_filename );

	// that's awesome, just like most modern languages, another benefit over the C way
	// they should have retro'd that into the language proper
	if "Kevin" == "Kevin" {
		print( "Would be cool is string comparisons just worked\n" );
	}

	one_span := New([5]int);
	print( "%\n", type_of(one_span) );


	values: [4] int;
	for i: 0..values.count-1 {
		values[i] = (1 << i);
	}

	print( "Values == %\n", values );

	// we want to say something like
	// bit = offsets[grade]

	// the offset for one should be 0, then 3, then 6

	// this builds the list of offsets

	dimension: int = 4;
	num_basis_elements: [4] int; // these need to be dimension + 1
	offsets: 	  		[4] int; // dimension + 1; cumulative offsets

	for i: 0..dimension-1 {
		num_basis_elements[i] = choose( n = dimension, k = i+1 );
	}

	offsets[0] = 0;
	for i: 1..dimension-1 { // skip over grade zero and one
		offsets[i] = offsets[i-1] + num_basis_elements[i-1];	
	}

	print( "%\n", num_basis_elements );
	print( "%\n", offsets );

	grade1: int = 1;
	grade2: int = 2;
	grade3: int = 3;
	grade4: int = 4;

	// can only work up to dimension 6 in this representational form

	print( "%,%,%,%\n", offsets[grade1-1], offsets[grade2-1], offsets[grade3-1], offsets[grade4-1] );

	// so when you have to get the grade, you have to use an offset
	// when you get the basis_component
	// get_basis_elements( vector.grade ) --> offsets
	// or what you do is have separate elements 
	//
	// [One element] --> [Two element]
	// then you just map directly into their bit vectors...

	
	
	return;
}

#scope_file
#import "Basic";
#import "File";
#import "Windows";



