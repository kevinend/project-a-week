#scope_export


/* What is the play here kev?
  --We want to recursively descend and generate the initial expression tree

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


// e1 ^ e2e3
// e2e3 ^ e1 // this should be positive right?
// e3 ^ e1^e2 // see you have to hop over yourself first

// e2e3 ^ e1
// e3e2
// e2 ^ e1e3 (2 ^ 5)

// so you hop over yourself which 
// so that is e2 over e3
// or we do this but start at the reverse end

*/

power :: ( a: int, b: int ) -> (c: int) {

	c: int = 1;
	for i: 0..b-1 {
		c *= a;	
	}

	return c;
}

main :: () {

{
	values: [..] int;
	array_add( *values, 1 );
	array_add( *values, 2 );
	array_add( *values, 3 );

	// could do something like this
	// could do i: queue.head..queue.num_entries
	// the operators is a separate index though that is 
	// it would be i and i+1 for the rvalue

	i := 0;
	count := values.count;
	while i < count { // this gets evaluated each time
		defer i += 1;
		print( "values[%] == %\n", i ,values[i] );
		array_add( *values, i + 10 );
	}
	print( "Value of i is %\n", i );

	count = values.count;
	while i < count {
		defer i += 1;
		print( "values[%] == %\n", i ,values[i] );
	}
	print( "Value of i is %\n", i );

	lvalue = elements[0];
	i     := 0;
	count := operators.num_entries;
	while i < count {
		op     = operators[i];
		rvalue = elements[i+1];
		if op == #char "^" {
			lvalue = exterior_product( lvalue, rvalue );
		}
		else
		if op == #char "+" || op == #char "-" {
			array_add( *operators, op );
			array_add( *elements, lvalue );
			lvalue = rvalue;
		}
	}

	count = operators.num_entries;
	while i < count {
		op, _ = dequeue( operators );
	}

	op_count := operators.num_entries;
	for i: 0..op_count {
	}

};

	x := power( 2, 4 );
	print( "%\n", x );

	for i: 0..3 {
		for j: i..3 {
			print( "i=%,j=% product = %\n", i, j, i*j );
		}
	}

	// e1 e2 e3 e4
	// 0  1  2  4
	// 
	// Index: 0  1  2  3
	// Basis e1 e2 e3 e4
	// Value: 1  2  4  8
	// 0-15, you have 16 different combos

	// dimension = 3 
	//  1 // grade 0
	//  3
	//  3
	//  1 = 7
  
	/*
		e1,e2,e3,e4
	*/

	// start from the right of the left argument and the left of the right argument
	// that way you have the correct number of swaps

	dimension: int = 4;
	left: int  = 6; // e1
	right: int = 1; // e2

	result: int = right; // e2

	// set the result = right
	num_swaps: int = 0;
	for < i: dimension-1..0 {
		if left & (1 << i) {
			print( "Left set == %\n", i );
			for j: 0..dimension-1 {
				if j == i {
					result = result | (1 << j);
					break;
				}
				else
				if result & (1 << j) {
					num_swaps += 1;
				}
			}
		}
	}

	print( "Number of swaps == %\n", num_swaps );
	print( "Result == %\n", result );

	if num_swaps % 2 != 0 {
		// odd number of swaps, negate the number
		print( "Should be negative\n" );
	}
	else {
		print( "Should be positive, leave untouched\n" );
	}

	return;
}

#scope_file
#import "Basic";



