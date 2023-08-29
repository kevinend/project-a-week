#scope_export

K_Element :: struct {
	grade: int;
	coefficient: int;
	one_element_components: s64;
}

Multi_Element :: struct {
	elements: [] K_Element;
}

EVAL_TEMP_BUFFER_LENGTH :: 8; // this needs to depend on the dimension of the space
temp_buffer: [EVAL_TEMP_BUFFER_LENGTH] K_Element;
temp_cursor: int;

add_multi_element :: ( grade: int, coefficient: int, one_element_components: s64 ) {

	assert( temp_cursor < EVAL_TEMP_BUFFER_LENGTH );

	temp_buffer[temp_cursor].grade 		 = grade;
	temp_buffer[temp_cursor].coefficient = coefficient;
	temp_buffer[temp_cursor].one_element_components = one_element_components;

	temp_cursor += 1;

	return;	
}

new_multi_element :: () -> ( result: *Multi_Element ) {

	// takes the elements out of the temp buffer, copy them to the heap, point our multi-element at that buffer
	// the multielement takes ownership of the memory and is responsible for freeing

	m := New(Multi_Element);
	m.elements.data  = alloc( temp_cursor * size_of(K_Element) );
	m.elements.count = temp_cursor;

	assert( m.elements.data != null );

	memcpy( m.elements.data, *temp_buffer[0], temp_cursor * size_of(K_Element) );

	temp_cursor = 0; // reset for the next set of multielements to be created

	return m;
}

eval :: ( node: *Ast_Node ) -> ( result: *Multi_Element ) {

	result: *Multi_Element;
	if node.kind == .K_Element {
		k_element_node := cast(*K_Element_Node)node;
		add_multi_element( k_element_node.grade, k_element_node.coefficient, k_element_node.one_element_components );
		result = new_multi_element();
	}
	else
	if node.kind == .Literal {
		literal_node := cast(*Literal_Node)node;
		add_multi_element( 0, literal_node.value, 0 );
		result = new_multi_element();
	}
	else
	if node.kind == .Unary_Operator {
		unary_operator_node := cast(*Unary_Operator_Node)node;
		operand := eval( unary_operator_node.operand );
		result = eval_unary_operator( unary_operator_node.operation, operand );
	}
	else 
	if node.kind == .Binary_Operator {
		// might want to check for errors or something after left and right???
		binary_operator_node := cast(*Binary_Operator_Node)node;
		left  := eval( binary_operator_node.left\_operand );
		right := eval( binary_operator_node.right_operand );

		result = eval_binary_operator( binary_operator_node.operation, left, right );
	}

	return result;
}


#scope_file

eval_unary_operator :: ( operation: Unary_Operation, operand: *Multi_Element ) -> ( result: *Multi_Element ) {

	// For a given multi-element the unary operators generate the same number results
	// that already belong to the multi-element so we update in place.
	// 
	// Example:
	//  Negate e1 => -e1
	//  Left  Complement e1,e2 => -e2,e1
	//  Right Complement e1,e2 =>  e2,e1

	result := operand;
	if operation == {
		case .Negation; { 
			for i: 0..operand.elements.count-1 {
				operand.elements[i].coefficient *= -1;	
			}
		}
		case .Left_Complement;  { 
			for i: 0..operand.elements.count-1 {
				operand.elements[i] = eval_complement( .Left, *operand.elements[i] );	
			}
		}
		case .Right_Complement; { 
			for i: 0..operand.elements.count-1 {
				operand.elements[i] = eval_complement( .Right, *operand.elements[i] );
			}
		}
	}

	return result;
}

eval_sum :: ( lvalue: *Multi_Element, rvalue: *Multi_Element ) -> ( result: *Multi_Element ) {
	
	assert( (lvalue.elements.count + rvalue.elements.count) < EVAL_TEMP_BUFFER_LENGTH );

	memcpy( temp_buffer.data, rvalue.elements.data, rvalue.elements.count * size_of (K_Element) );

	// initial implementation; slow!
	for i: 0..lvalue.elements.count-1 {
		for j: 0..rvalue.elements.count-1 {
			if lvalue.elements[i].one_element_components == rvalue.elements[j].one_element_components {
				temp_buffer[j].coefficient += lvalue.elements[i].coefficient;
			}
		}
	}

	result := new_multi_element();
	return result;	
}

eval_difference :: ( lvalue: *Multi_Element, rvalue: *Multi_Element ) -> ( result: *Multi_Element ) {

	result: *Multi_Element;
	for i: 0..rvalue.elements.count-1 {
		rvalue.elements[i].coefficient *= -1;
	}

	result = eval_sum( lvalue, rvalue );
	return result;
}

exterior_product :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( result: K_Element ) {

	assert( temp_cursor < EVAL_TEMP_BUFFER_LENGTH );

	result: K_Element;
	result.grade = 0;
	result.coefficient = 0;
	result.one_element_components = 0;

	if lvalue.grade == 0 || rvalue.grade == 0 {
		result.grade 	   			  = lvalue.grade + rvalue.grade;
		result.coefficient 			  = lvalue.coefficient * rvalue.coefficient;
		result.one_element_components = lvalue.one_element_components | rvalue.one_element_components;
	}
	else
	if (lvalue.one_element_components & rvalue.one_element_components) == 0 { // no one elements in common; implies sum of grades is less than or equal to N	
		result.grade				  = lvalue.grade + rvalue.grade;
		result.coefficient			  = lvalue.coefficient * rvalue.coefficient;
		result.one_element_components = lvalue.one_element_components | rvalue.one_element_components;
	}

	return result;
}

eval_exterior_product :: ( lvalue: *Multi_Element, rvalue: *Multi_Element ) -> ( result: *Multi_Element ) {

	// need to handle distribution
	// (e1,e2,e3)^(e1,e2,e3) is 9 results, though some may be zero!

	assert( (lvalue.elements.count * rvalue.elements.count) < EVAL_TEMP_BUFFER_LENGTH );

	k_element: K_Element;
	for i: 0..lvalue.elements.count-1 {
		for j: 0..rvalue.elements.count-1 {
			k_element = exterior_product( *lvalue.elements[i], *rvalue.elements[i] );
			add_multi_element( k_element.grade, k_element.coefficient, k_element.one_element_components );
		}
	}

	result := new_multi_element();
	return result;
}

// when you do interior product you have to do the k-span of equal grade
// so we will need that product somewhere in here
// e123
// e12 e2e3
// e1e2^e2 reg e3 + e1e2^e3 reg e2
//
// for each element in the left
//  for each element in the right  (could probably cache this..., nope don't know the arguments)
//    generate the k-span based on the right and left arguments (list K-Elements)
//    generate the list of k-cospan elements (list K-Elements)
//    regressive_product( k-span, k-cospan )
//    add to the temporary buffer
//    copy out into the products buffer
//    then need to sum that buffer
//  e1,e2 reg e1,e2
//    then you need to sum in the results buffer, add one, then 
//
// if you can do one, you can do many
// so how do you do one?
//
//  one element and one element
//  if grade of left + grade of right is less than N, no spanning required, return 0
//  else 
//   span right element which generates a list of k-elements (this is determined from)
//   cospan right elements which generates a list of k-cospan elements
//   iterate through that list pairwise and do the actual regressive product
//   take each of those products and sum them up
//   
//  so that is two multielements composed of single elements
//  what if a multielement has two elements
//   (e1)  (e1,e2)
//  temp_k_span:
//  temp_k_cospan:
//  temp_products: [], 
//  temp_sums:     [] add the first element from the product
//  then you generate a new set of products
//  add that to the running list of total sums
//
//  or you use that trick to find the common factor
//  it would generate way fewer products and much smaller footprint
// e1 _e1e2 = e2

//  e1e2 e2e3
//  that is to generate a set of factors though

// so i had this thought last night as well
// we the interior or dot product is regressive product of the complement so we can do the same
// thing without the full expansion

eval_regressive_product :: ( lvalue: *Multi_Element, rvalue: *Multi_Element ) -> ( result: *Multi_Element ) {

	// implements the Grassmann equivalent of Demorgan's Law
	//    NOT(e1 regressive e2) = NOT(e1) ^ NOT(e2)
	// ...    e1 regressive e2  = NOT(NOT(e1) ^ NOT(e2))
	// to see expansion using k-span and k-cospan, look at the expand function

	assert( (lvalue.elements.count * rvalue.elements.count) < EVAL_TEMP_BUFFER_LENGTH );

	for i: 0..lvalue.elements.count-1 {
		lvalue.elements[i] = eval_complement( .RIGHT, *lvalue.elements[i] );
	}

	for i: 0..rvalue.elements.count-1 {
		rvalue.elements[i] = eval_complement( .RIGHT, *rvalue.elements[i] );
	}

	// distribute, and then need to sum them up (that part I don't have figured out yet)
	// since i have the tree expansion, I might just want to do it. It might make more sense to do so
	k_element: K_Element;
	for i: 0..lvalue.elements.count-1 {
		for j: 0..rvalue.elements.count-1 {
			k_element = exterior_product( *lvalue.elements[i], *rvalue.elements[j] );
			k_element = eval_complement( .RIGHT, k_element );

			for k: 0..temp_cursor-1 {
				if temp_buffer[k].one_element_components == k_element
			}
		}
	}

	// still convert the products, however we at least get a tree that handles all this stuff for us
	// expand, then simplify
	// i still like the concept of multi-elements
	// then we just have the sums and you take the elements and if they don't sum oh well
	//      +
	//   e1  +
	//      e2 e3
	// convert each multielement then you say if you iterate through here
	// if you do the then you have e1 + e2 + e3 which is the order you have
	// it would be cool if we didn't have to do the iterative loop though
	//
	// parse --> expand --> simplify/eval
	// partial eval for the exterior product in the tree
	// but that is just what we need to do 
	

	// iterate through the temp buffer of products and try to sum them up

	//  (e1,e2) | (e1,e2)
	//  (>>e1,>>e2) | (>>e1,>>e2)
	// add the product to a list of sums
	// if you put the products into the temp_buffer, then you run a sum across those


	// this procedure is the big kahuna, if i can get this to work then we are golden
	// or we use the Demorgan's law to flip all of these and just wedge products?
	// fewer terms that way

	// (e1,e2)| (e3,e4)
	// what needs to be done:
	//	(1) distribute the product
	//  (2) generate the k-span
	//  (3) generate the co-span
	//  (4) evaluate the regressive_product
	//  (5) return
	//
	// Alternative (that saves a lot of space)
	// Use equivalent of demorgan's law to save a shit ton of space
	// Not(e1^e1) 	  = Not(e1) reg Not(e2)
	// Not(e1 reg e2) = Not(e1) ^ Not(e2)
	// e1 reg e2      = Not(Not(e1) ^ Not(e2))

	

}

eval_binary_operator :: ( operation: Binary_Operation, lvalue: *Multi_Element, rvalue: *Multi_Element ) -> ( result: *Multi_Element ) {
	
	// there should only be a single basis element in each multielement

	result: *Multi_Element;
	if operation == {
		case .Sum; { 
			result = eval_sum( lvalue, rvalue ); 
		}
		case .Difference; {
			result = eval_difference( lvalue, rvalue );
		}
		case .Exterior_Product; {
			result = eval_exterior_product( lvalue, rvalue );
		}
		case .Regressive_Product; {
			result = eval_regressive_product( lvale, rvalue );
		}
	}

	return null;
}

Complement_Variant :: enum {
	Left;
	Right;
}

eval_complement :: ( variant: Complement_Variant, k_element: *K_Element ) -> ( n_k_element: K_Element ) {

	n_k_element: K_Element;
	n_k_element.grade 		= space.dimension - k_element.grade;
	n_k_element.coefficient = k_element.coefficient;
	n_k_element.one_element_components = space.n_element ^ k_element.one_element_components;

	// Need to determine whether the (n-k)-element should be negated.
	// The procedure below effectively takes the exterior product between the args and determines the number
	// of swaps required to build the final N element from the product of the (n-k)-element or the k-element
	// or vice versa depending on whether this is a left or right complement.

	if variant == .Left {
		n_k_element.coefficient *= anticommutative_coefficient( *n_k_element, k_element );
	}
	else
	if variant == .Right {
		n_k_element.coefficient *= anticommutative_coefficient( k_element, *n_k_element );
	}

	return n_k_element;
}

anticommutative_coefficient :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( coefficient: int ) {

	// This procedure calculates the number of swaps required to reorder the elements into the 'canonical' ordering to build the N-element.
	// If the number of swaps is odd then the product needs negated.

	// The current implementation forces an ordering of 1-elements such that the exterior product of (e2^e1)^e3
	// will automatically be reordered to the positive (scalar factor is positive) product e1^e2^e3.

	// Algorithm:
	// Take the exterior product between the elements and handle the anticommutativity property of the grassmann product.
	// For every bit (basis one element) in the LEFT argument
	//	Start at right-most bit in the LEFT arg
	//  Find it's place in the RIGHT arg and set that bit.
	//	Anytime you cross over a bit in the RIGHT that is set, then you have swapped the elements.
	// Rinse and repeat for every other bit in the LEFT.
	//
	// If after completing this process we have swapped an odd number of times then we negate the final product.

	temp: int = rvalue.one_element_components;
	num_swaps: int = 0;
	for < i: space.dimension-1..0 {
		if lvalue.one_element_components & (1 << i) {
			for j: 0..space.dimension-1 {
				if i == j { 
					temp |= (1 << j);
					break;
				}
				else 
				if temp & (1 << j) { 
					num_swaps += 1;
				}
			}
		}
	}

	if num_swaps % 2 == 0 { return 1; }
	else { return -1; }
}
