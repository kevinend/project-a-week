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

// N = 4
// 1-4=  4
// 2-6= 12
// 3-4= 12
// 4-1=  4
//   32 iterations
// 4 * 15 = 60
// gray codes 
// 0, have to iterate over each of these
// 1, 
// 2, iterate through 2
// 3 iterate through 4
// 8 iterate through all 8
// 14 iterations, though you need to go through the call and manage the memory
// for this, but you do get the grades for free

// so if we allocate a buffer of 16 bits
// then we allocate the numbers 0 and 1
// then we iterate through that list
// and add a bit to the left and a bit to the right
// 00 01 10 11 // grade 2
// 000 001 010 001 011 101 110 can't keep track of grades this way like i thought
// forget that just do the pop count
// you can update the data as  you go
// 00 01 10 11 -> as you generate you lookup in the table and say these are the one elements

// This does a lot of work to build the basis elements but it does it once
// N = 4; total iterations = 32 to get grade
// N = 5; total iterations = 81 to get grade
// N = 6; total iterations = 193 to get grade

pop_count :: ( x: int ) -> ( count: int ) {

	count: int = 0;
	while x != 0 {
		count += 1;
		x &= x-1;
	}

	return count;
}

generate_basis_symbol :: ( element: int, grade: int, n: int, one_element_symbols: [] string ) -> ( symbol: string ) {

	builder: String_Builder;
	num_processed_basis_one_elements: int = 0;

	if grade == 0 {
		// this might not be the correct play when printing one-elements
		append( *builder, "1" );
	}
	else {
		// Iterate through each bit and lookup the corresponding basis symbol supplied by the user
		// This is slow when building the entire set of basis elements for the user however this code path
		// is traversed maybe once or twice during a run of the application so willing to put up with this for now.
		// Also if the dimension (N) starts creeping up then this gets even slower but it will for sure work, which
		// was not guaranteed with the other approaches I took.
		
		for i: 0..n-1 {
			if element & (1 << i) {
				append( *builder, one_element_symbols[i] );
				if num_processed_basis_one_elements < grade-1 { // in an expression like e1^e2 the number of exterior products is the grade-1
					append( *builder, "^" );
				}
				num_processed_basis_one_elements += 1;
			}
		}
	}

	// TODO: Leaking memory!!!
	symbol: string = builder_to_string( *builder );
	return symbol;
}

main :: () {

{
	// so now you can store the elements as a coefficient
	// this implements the flyweight pattern!
	//
	// so a k-element is a coefficient, element: int; which is just a lookup into the table
	// basis_element:
	// then you can take the basis_element exterior basis_element
	// k_element.basis ^ n_element.basis
	// something like that maybe?
	// then when you print you can do something like basis_elements[e1].symbol + basis_elements[e2].symbol
	// or something like that which is pretty cool

	power :: ( n: int, k: int ) -> ( value: int ) {
		value: int = 1;
		for i: 0..k-1 {
			value *= n;
		}

		return value;
	}
	bits: int;
	grade:  int;
	symbol: string;
	dimension: int = 3;
	one_element_symbols := string.["e1","e2","e3"];

	num_basis_elements: int = power( 2, dimension );

	for i: 0..num_basis_elements-1 {
		grade := pop_count(i);
		symbol = generate_basis_symbol( i, grade, dimension, one_element_symbols );
		print( "(i,grade,symbol) == (%,%,%)\n", i, grade, symbol );
	}
}

// should be able to apply the same idea with the k-span to generate all the elements
// something like this should work, so you allocate space to store all of these up front based on the dimension
{
	choose :: ( n: int, k: int ) -> ( value: int ) {
		// example: choose(4,2) == (4*3)/(2*1) == 6
		if k == 0 return 1;
		return (n * choose(n-1, k-1)) / k;
	}

	power :: ( n: int, k: int ) -> ( value: int ) {
		value: int = 1;
		for i: 0..k-1 {
			value *= n;
		}

		return value;
	}

	n: int = 3;
	// user supplies the list of basis symbols
	basis_symbols := string.["e1","e2","e3"];
	print( "Basis symbols count == %\n", basis_symbols.count );

	num_basis_elements: int = power( 2, n );
	print( "Num basis elements == %\n", num_basis_elements );

	Basis_Element :: struct {
		grade: int;
		symbol: string;
	}

	basis_elements: [] Basis_Element;
	basis_elements.data  = alloc( num_basis_elements * size_of(Basis_Element) );
	basis_elements.count = num_basis_elements;

	assert( basis_elements.data != null );

	basis_elements[0].grade = 1;
	basis_elements[0].symbol = "e1"; // didn't crash so that is good

	
	// then we populate this bitch with the values we create below
	// all we do is allocate a max of 2^6 which is 64 of these bad boys and use what we need so we don't have
	// to keep reallocating all the time
	
	// so our approach for the 'span' doesn't actually work
	// but we can do something a little bit more clever
	//
	// Grade N
	// Grade N-1
	// Grade 1
	// from the grade 1 we can easily build the Grade-1 elements
	// then the N-1 elements we iterate through the one elements and remove each 1-element one at a time
}

/*
	and you're like hey dude where is (3,11)?
	(3,7)
	(3,13)
	(3,14)

	but i think we can generate that rather easily

	and what do we have here?
	these are all the three elements, kablamy
	(7)
	(11)
	(13)
	(14)

	then from the three elements you want to store the two elements
*/

	return;
}

#scope_file
#import "Basic";
#import "File";
#import "Windows";
#import "String";

