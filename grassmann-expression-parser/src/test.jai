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

/*
	// so the default now is that we just read an identifier
	// but then we need to make a determination whether it is a keyword 
	// or if it is a basis symbol 

	// so an identifier can either be an identifier
	// lex
	// parse (set context)
	// checks the list to see if it is a basis element, otherwise it has to do a lookup (has to do it anyways)

	Dimension = 3;
	Basis_Symbols = [e1,e2,e3];
	Basis_Symbols = [x]; // expand to x1,x2,x3
	Basis_Symbols = [x,y,z]; 

	Deciding on how this should all work, i had a bunch of keywords but i don't think they are necessary
	The flow would be something like this.

	L1 = Whatever;
	L2 = Whatever;
	L3 = L1 + L2;
	L3; // this forces an eval

	For keywords:
	Dimension = 3; 				// parser recognizes this and immediately executes the Algebra_Context, generates the basis symbols e1,e2,e3
	Basis_Symbols = [e1,e2,e3]; // could default to G(R4) with basis symbols whatever
	// so that those symbols get recognized in the parser

	For identifiers we want to keep track whether they need re-evaluated or not
	which is just a simple flag.
*/

// op codes 
// so in his book
// lexer --> parser --> compile --> interpreter
// [string] --> [tokens] --> [AST] --> [ByteCode] --> [Output]
//  ^----------------------^ then some loop back here for some reason that I didn't understand


// good deal i think this is a good stopping place for now. Need to look at the parser
// maybe put this stuff in a lexer.jai file for grassmann
// think about file structure a bit more
/*
	grassmann.jai
	lexer.jai
	parser.jai
	eval.jai  

	// maybe three sets of files? i don't know man just hanging around!!!

*/

/*
	Executive decision, ultimately create font to create the glyphs
	Have a little window where you select the characters or some key mapping to pick from there

	Like Ctr+E or Alt+E for the exterior product and store that
	We are going to introduce symbols for now
	
	Going to use logical symbols for now
	<< e1^e2 is the left complement
	>> e1^e2 is the right complement
	regressive product is a compound symbol it is !^, it is the opposite of the exterior product

	(<< e1^e2^e3 !^ >> e1^e2) // definitely too messy for my liking but need to roll with it for now!
*/

/*
	What is the current problem?
	We want to introduce another step, compilation, prior to evaluation in our process.
	In the compilation step we MUST be able to issue an operand and have it result in a value
	So when you say something like:
		OpAdd 1 3 then you get the result 4 on that stack machine
		OpSub 1 4 then you get 3

	In our case our 'objects' will be different, they will be K-Elements but it's possible that the elements
	can't be directly summed
	OpAdd e1 e2 ; what does this resolve to? do we return a string and allocat this out again or something?
				  doesn't seem quite right

	We don't have a representation for a Multi-Element or define what addition is among a Multi-vector
	It is a pairwise assignment of coefficients but it isn't defined in the lexer or parser right now
	We also don't have the ability to recognize a symbol like e1^e2^e3 as a single element
	Ideally we'd like to be able to say e1e2e3 and have that be recognized as a single symbol
	We just represent everything as forms of basis-one elements

	Which doesn't necessarily seem wrong to me but we have to handle the multi-vectors on the other end somehow

	So i thought if we got the dimension and the list of basis one elements would could build the basis elements 
	
	e1
	e2
	e3
	e4
	e1e2
	e1e3 ...

	then when we are lexing we could recognize the elements
	if it isn't recognized as a single symbol then it is investigated further to separate out the component parts

	so you would have a table that is like this

		8, 16, 32, 64
		1      0
		e1	   1   (1 << 1)
		e2     2   (1 << 2)
		e3     3   (1 << 3)
		e1e2   4   (1 << 4)
		e1e3   5   (1 << 5)
		e2e3   6   (1 << 6)
		e1e2e3 7   (1 << 7)

		and these indices represented the index, so you have a field that is like basis-elements

		and you return the basis_elements as a bit field
			e1, e1e2
			the first bit and the 4th bit are set or something to that effect

			which ultimately represents a multivector which is the sum
			so give that element a grade of -1 (since it's grade is technically undefined)
			when you wedge a product over it, you'd have to generate a list of products
			^
		K-Element Multi-Grade-Element

		you'd have to issue instructions to take the K-Element over the first element and add instruction to the second
		^ LEFT RIGHT-FIRST // wherever you stored this
		^ LEFT RIGHT-SECOND // wherever you stored this
		+ a b
		// which if they are K-Elements then you get the summed result back
		// the thing that sucks if you need to track coefficients for all these bad boys, hmm
		// don't want to store a bunch of zeroes all over the place
		
		??? How do you perform the actual exterior/wedge product? 
		since before we just did an OR and that was simple but in this case if you have
		e1 and e2 and wedge them together then you don't get the correct result
		need to clear the bits
		and then it is not obvious that e1e2 ^ e1 is zero anymore

		can we use a combination of both ideas?
		it ultimately is a list of elements, not sure how else you could faithfully represent it

		so right now we punt on it but we would need a list of elements and their coefficients
		and their grades

		you probably end up with the same amount of space used and it literally is what it is
		you need to faithfully represent all these objects

		so instead of distribute the grassmann products you would have to build a thing

		e1 ^ (e1 + e2) this turns into a Multi-Element
			^
		  e1 +
		    e1 e2

		    ^
		  e1 Multi-Element which has a list of K-Element

		  then the result is itself a multi-element whose grade is degree 2
		  you need procedures that tell you whether it is a bivector or trivector
		  it is either K-Element 

		  a Multi-Element is either a K-Element or a linear combination of K-Elements of differing grades

		  Multi-Element: e1 e2
		  Multi-Element: e1^e1 + e1^e2 then this list collapses down to 0 + e1^e2

		 so you handle the distribution there instead of as a pass over the data

		 Multi-Element :: struct {
		 	#as using _kind: Ast_Node;
			elements: [] K_Element; // since these are all the same size we could just pool them
									// put them next to each other in memory
		 }

		 they actually won't exist in the parser, cause we will issue commands to turn them into these things
		 so they would exist in the interpreter when calculating final values

		 but we could introduce them into the 'grassmann-parser.exe' and see how that goes
		 since we don't have a working VM yet


		FROM DA BOOK:
		 Define the opcode and how it is encoded
		 Augment the compiler to emit the instruction
		 Update the VM to decode and execute the instruction
		 Rinse and Repeat

		 Stack-based VM since so it needs to have instructions to push and add and stuff like that


*/

main :: () {

{
	// e1 + e2 + e1
	values: [4] int;
	order:  [4] u8;
	num_elements: int = 0;

	values[1] = 1;
	values[2] = 1;

	order[0] = 1; // index
	order[1] = 2;

	num_elements = 2;

	for i: 0..num_elements-1 {
		if order[i] == 1 { print( "Break" ); break; }
		else {
		}
	}
}

{
	// so we can work with scratch k-element nodes when doing the operations
	// i'd like to turn eval into a tree-walking, with the new restructured format and multi-elements
	// THEN revisit doing it as a VM that compiles instructions
	//
	// so we have a working implementation that we can fall back on
	// there is some partial evaluation that has to happen before the instructions are issued though
	// otherwise we have to
	// eval would have to issue a span command
	// which could generate a lot of instructions (i'd have to think about it)
	// we would have k-span, k-cospan and the like that would have to happen on the instruction side of things
	//
	// it seems to me like it could get complicated
	// then sum them up or something, hmm
	

	scratch: [5] int;
	num_values:  int;
	
	scratch[0] = 10;
	scratch[1] = 20;
	scratch[2] = 30;
	scratch[3] = 40;
	scratch[4] = 50;
	num_values = 3;

	values: [] int;
	values.data = alloc( num_values * size_of(type_of(scratch[0])) );
	values.count = num_values;

	memcpy( values.data, scratch.data, num_values * size_of(type_of(scratch[0])));

	print( "Values == %\n", values );
	free( values.data );
}

/*
{
	Instruction_Encoder :: struct {
		temp_buffer: int; // push the instruction and the operands, 	
	}

	// could just push the op code into the buffer
//	encode_instruction :: ( op: OpCode ) {
//
//	}

	encode_one_operand_instruction   :: (op: OpCode, operand0: u8 ) { }
	encode_two_operand_instruction   :: (op: OpCode, operand0: u8, operand1: u8 ) { }
	encode_three_operand_instruction :: (op: OpCode, operand0: u8, operand1: u8, operand2: u8 ) { }

	Instructions :: #type [..] u8;

	instructions: Instructions;
	array_add( *instructions, 10 );

	for instruction: instructions {
		print( "Instruction == %\n", instruction );
	}

	OpCode :: enum u8 {
		
		// index into the definitions table

		OpError    :: 0;
		OpConstant :: 1; // 'constant' expression
		OpAdd 	   :: 2;  
		OpSub 	   :: 3;
	}

	OpCode_Definition :: struct {
		name: string;
		operand_width_in_bytes: int; 
	}

	definitions: [] OpCode_Definition = .[
		.{"OpError", 	-1 },
		.{"OpConstant",  1 },
		.{"OpAdd",    	 2 },
		.{"OpSub",    	 1 }
	];

	get_opcode_definition :: ( op: OpCode, definitions: [] OpCode_Definition ) -> ( found: bool, definition: OpCode_Definition ) {
		if cast(u8)op <= definitions.count {
			return true, definitions[op];
		}
		else {
			return false, definitions[0];
		}
	}

	definition := definitions[OpCode.OpAdd];
	print( "Definition name and width == % %\n", definition.name, definition.operand_width );

	definition = definitions[OpCode.OpSub];
	print( "Definition name and width == % %\n", definition.name, definition.operand_width );

	// cool what was he doing with this where he needed a slice and a map???
}
*/

{
	values: [..] int;
	for i: 0..4 {
		array_add( *values, i+1 );
	}

	indirect: [] int = values; // does that typecheck
	print( "Indirect == %\n", indirect );

	// what happens if we add something after the assignment?
	array_add( *values, 10 );

	indirect = values;
	print( "After update == %\n", indirect );

	// we'd have to set it on every loop, which is a bit wasteful but whatever
}

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

{
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
}
	
	
	return;
}

#scope_file
#import "Basic";
#import "File";
#import "Windows";
#import "String";

