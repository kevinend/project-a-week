#scope_export


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

// what i set up is cute but it isn't exaclty what you would want to do
// well i'm honestly not sure,
// could you have a k-vector and just that?
// or should we have different types, in reality it is a single integer value
// in either case, so maybe it isn't terrible

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

// so you have the mapping but how do you do things like complements?

/*
	e1	  e2   e3
    e1e2 e1e3 e1e2 (maybe it is easier because you just move both at the same time), how many bits are set in this one, move both and call it a day
    also you don't know exactly how many N-vectors you will have generically speaking

    you know you have some k-vectors, some of those might be vectors, some might be bivectors, you might not have trivectors
    then you need to account for them, it honestly is probably easier to go generic K_Vector route
*/

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

span :: ( values: [] int, span: int ) -> ( k_span: [..] int ) {

	k_span: [..] int;

	span_element: int;
	slice: [] int;
	for i: 0..values.count-1 {
		base: int = values[i];
		span_element = base;
		for j: i+1..values.count-1 {
			slice.data  = *values[j];
			slice.count =  values.count-1;

			k: int = 0;
			while k < (span-1) && slice.count >= (span-1) {
				span_element += slice[k];
				k +=1;
			}

			if k == span-1 {
				array_add( *k_span, span_element );
				span_element = base; // reset to the base 1-element
			}
		}
	}

	return k_span;
}

// 1, 2, 4, 8



main :: () {

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

	k_span := span( values, 2 );
	print( "k_span == %\n", k_span );

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



