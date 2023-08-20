#scope_export

// Not sure what to do with this stuff. It is needed by the lexer but the user may request to see the declared space
// or the basis elements or the n-element, so stuffing in a separate file.

Grassmann_Space :: struct {
	dimension: int;
	basis_one_elements: [..] string;
	n_element: int;
}

set_grassmann_space :: ( space: *Grassmann_Space, N: int, basis_one_elements: [] string ) {
	
	// Invoked on application startup with a set of defaults.
	// When user enters certain keywords this procedure is invoked to update
	// the properties of the 'space'.

	// set the dimension
	space.dimension = N;

	// initialize the basis_one_elements for the space and build the n-element
	if space.basis_one_elements.count > 0 {
		array_reset( *space.basis_one_elements );
	}

	space.n_element = 0;
	for i: 0..N-1 {
		array_add( *space.basis_one_elements, basis_one_elements[i] );
		space.n_element |= (1 << i);	
	}

	return;
}

// the parser shouldn't need anything from it but the evaluator will
