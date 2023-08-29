#scope_export

K_Element :: struct {
	grade: 		 int = 0;
	coefficient: int = 0;
	one_element_components: s64 = 0; // bit-vector
}

K_Span_Element   :: #type K_Element;
K_Cospan_Element :: #type K_Element;

Complement_Type :: enum u8 {
	Left;
	Right;
}


is_regressive_product :: inline ( node: *Binary_Operator_Node ) -> ( yes: bool ) { 
	
	return node.op == .Regressive_Product;
}

is_grassmann_op :: inline ( node: *Binary_Operator_Node ) -> ( yes: bool ) {

	yes: bool = false;
	if node.op == {
		case .Regressive_Product; #through
		case .Exterior_Product;   #through
		case .Interior_Product;   { yes = true; }
	}

	return yes;
}

is_arithmetic_op :: inline ( node: *Binary_Operator_Node ) -> ( yes: bool ) {
	
	yes: bool = false;
	if node.op == {
		case .Sum; 		  #through
		case .Difference; { yes = true; }
	}

	return yes;	
}


distribute_unary_operator  :: ( unary_op, binary_op, lvalue, rvalue ) -> ( result: *Binary_Operator_Node ) {

	left\_node := new_unary_operator_node( unary_op, clone_node( lvalue ) );
	right_node := new_unary_operator_node( unary_op, clone_node( rvalue ) );
	
	binary_operator := new_binary_operator_node(
		 binary_op
		,expand( left\_node )
		,expand( right_node )
	)

	return binary_operator;	
}

simplify :: ( node: *Ast_Node ) -> ( ast: *Ast_Node ) {

	// This procedure can be called on any node in an expression tree.

	// Needs to handle sums and differences of equal grades (at the same level of the hierarchy)
	// Needs to handle exterior products
	// Needs to handle regressive products post CFT

	subtree: *Ast_Node;
	if node_is_k_element( node ) {
		subtree = clone_node( node );
	}
	else
	if node_is_unary_operator( node ) {
		unary_operator := cast(*Unary_Operator_Node)node;
		unary_operator.operand = simplify( unary_operator.operand );

		assert( unary_operator.operand.kind == .K_Element_Node );
		operand := cast(*K_Element_Node)unary_operator.operand;

		if unary_operator.op == {
			case .Left_Complement; {
				result = complement( .Left, operand );
			}
			case .Right_Complement; {
				result = complement( .Right, operand );
			}
			case .Negation; {
				operand.coefficient *= -1;
				result = clone_node( operand );
			}
		}
	}
	else {
		binary_operator := cast(*Binary_Operator_Node)node;
		binary_operator.left  = simplify( binary_operator.left  );
		binary_operator.right = simplify( binary_operator.right );

		if binary_operator.left.kind == .K_Element && binary_operator.right.kind == .K_Element {
			left  := cast(*K_Element_Node)binary_operator.left;
			right := cast(*K_Element_Node)binary_operator.right;

			op := binary_operator.op;
			if op == .Exterior_Product {
				result = exterior_product( *left.k_element, *right.k_element );
			}
			else
			if op == .Regressive_Product_Post_CFT {
				result = regressive_product( *left.k_element, *right.k_element );
			}
			else
			if op == .Sum || op == .Difference {
				// this only sums two leaf nodes that have the same one element components!
				// there is a proc to handle sums across all nodes in the tree that is called when calculating the final result
				// of an expression tree.
				
				if left.one_element_components == right.one_element_components {
					sum := clone_node( left );
					if op == .Difference { right.coefficient *= -1; }
					sum.coefficient += right.coefficient;
					result = sum;
				}
				// zero is the additive identity
				else
				if left.coefficient == 0 {
					if op == .Difference {
						right.coefficient *= -1;
					}
					sum := clone_node( right );
					result = sum;
				}
				else
				if right.coefficient == 0 {
					sum := clone_node( left );
					result = sum;
				}
				else {
					// sum is of two k-elements of different grades; can't simplify right now
					result = clone_node( node );
				}
			}
		}
		else {	
			// clone the node. Either left or right is a .Regressive_Product where the CFT needs applied
			// or one of the nodes is a .Sum or .Difference.
			result = clone_node( node );	
		}

	}

}

// implement: apply_common_factor_theorem

expand :: ( node: *Ast_Node ) -> ( ast: *Ast_Node ) {

	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.

	subtree: *Ast_Node;
	if node_is_k_element( node ) {
		subtree = clone_node( node );	
	}

	 
	if node_is_unary_operator( node ) {
		unary_operator := cast(*Unary_Operator_Node)node;

		// determine whether we need to distribute the operator or not
		need_to_distribute: bool = false;
		if node_is_binary_operator( unary_operator.operand ) {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;
			if binary_operator.operation == .Sum || binary_operator.operation == .Difference {
				need_to_distribute = true;
			}
		}

		if need_to_distribute {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;

			new_left\_node := new_unary_operator_node( unary_operator.operation, clone_node( binary_operator.left\_operand ) );
			new_right_node := new_unary_operator_node( unary_operator.operation, clone_node( binary_operator.right_operand ) );

			new_binary_operator := new_binary_operator_node(
				 binary_operator.operation
				,expand( new_left\_node )
				,expand( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}

	if node_is_binary_operator( node ) {
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.operation == .Regressive_Product {
			// determine if we need to apply the common factor theorem
			if binary_operator.left\_operand.kind == .Regressive_Product {
				new_left_operand := expand( binary_operator.left\_operand );
				binary_operator.left\_operand = new_left_operand;
			}
			if binary_operator.right\_operand.kind == .Regressive_Product {
				new_right_operand := expand( binary_operator.right\_operand );
				binary_operator.right_operand = new_right_operand;
			}

			left:  *Ast_Node = simplify( binary_operator.left );
			right: *Ast_Node = simplify( binary_operator.right );

			subtree = apply_common_factor_theorem( left, right ); 

			// do we want to run simplify over this again? this is crazy pants, or do we expand again?
			// holy christ this is absolute madness
		}
		else
		if binary_operator.operation == .Exterior_Product ||  binary_operator.operation == .Regressive_Product {
			// determine if we need to distribute
			left\_operator: *Binary_Operator_Node = null;
			right_operator: *Binary_Operator_Node = null;

			if node_is_arithmetic_operator( binary_operator.left\_operand ) {
				// distribute left node over the right
			}
			else 
			if node_is_arithmetic_operator( binary_operator.right\_operand ) {
				// distribute right node over to the left
			}
			if node_is_binary_operator( binary_operator.left\_operand ) {
				left\_operator = cast(*Binary_Operator_Node)binary_operator.left\_operand;
				if left\_operator == .Sum || left_operator.operation == .Difference {
					// distribute left over right
				}	
			}
		}
	}
	else {
		// Determine whether we need to distribute the binary grassmann products over any sums or differences.
		// The boolean values are introduced to reduce the level of nesting that happens if you don't 
		// do it that way (it was very hard to visually parse and a lot of duplicate 'clone the current node because
		// we don't need to distribute')

		binary_operator := cast(*Binary_Operator_Node)node;
		left\_operator: *Binary_Operator_Node = null;
		right_operator: *Binary_Operator_Node = null;

		distribute_over_left\_operand := false;
		distribute_over_right_operand := false;

		if operator_is_grassmann_product( binary_operator.operation ) {
			if node_is_binary_operator( binary_operator.left_operand ) {
				left_operator = cast(*Binary_Operator_Node)binary_operator.left_operand;
				if left_operator.operation == .Sum || left_operator.operation == .Difference {
					distribute_over_left_operand = true;
				}
			}
			else 
			if node_is_binary_operator( binary_operator.right_operand ) {
				right_operator = cast(*Binary_Operator_Node)binary_operator.right_operand;
				if right_operator.operation == .Sum || right_operator.operation == .Difference {
					distribute_over_right_operand = true;
				}	
			}
		}

		if distribute_over_left_operand {
			// distribute the argument on the left over the argument on the right
			grassmann_operator  := binary_operator.operation;
			arithmetic_operator := left_operator.operation;

			new_left\_node := new_binary_operator_node( 
				 grassmann_operator
				,clone_node( left_operator.left_operand )
				,clone_node( binary_operator.right_operand )
			);

			new_right_node := new_binary_operator_node(
				 grassmann_operator
				,clone_node( left_operator.right_operand )
				,clone_node( binary_operator.right_operand )
			);

			// to fully expand the subtrees we need to recursively call this procedure against the new left and right subtrees
			new_binary_operator := new_binary_operator_node(
				 arithmetic_operator
				,expand( new_left\_node )
				,expand( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else
		if distribute_over_right_operand {
			// distribute the argument on the left over the argument on the right
			grassmann_operator  := binary_operator.operation;
			arithmetic_operator := right_operator.operation;

			new_left_node := new_binary_operator_node(
				 grassmann_operator
				,clone_node( binary_operator.left_operand )
				,clone_node( right_operator.left_operand )
			);

			new_right_node := new_binary_operator_node(
				 grassmann_operator
				,clone_node( binary_operator.left_operand )
				,clone_node( right_operator.right_operand )
			);

			// to fully expand the subtrees we need to recursively call this procedure against the new left and right subtrees
			new_binary_operator := new_binary_operator_node(
				 arithmetic_operator // sum or difference
				,expand( new_left\_node )
				,expand( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}

	return subtree;
}


// expand_and_simplify( ast, interpreter )
//  distribute_grassmann_products( ast, interpreter )
//  
// hmm.. not obvious to me unfortunately

// so i noticed this too yesterday that I don't 'regressive' properly
// when you get a regressive product
//
// so say you have something like e1 !^ e2 !^ e3
//    R
//  e1  R
//    e2  e3
//
// when we hit the first R our code now says 'simplify' but what actually should happen
// is that you hit that first regressive product
// check the left and right
// if the left is a regressive product
//  then you need to recurse down into it, only when you have two vectors can you apply the CFT
//   if it is a sum then you need to distribute
//   

// CASES:
//   Stop at a regressive product
//    if LEFT AND RIGHT is a K-element then you apply the CFT
//    if LEFT is a PLUS then you want to distribute
//    if RIGHT is a PLUS then you want to distribute
//    (e1 + e2) R (e2 + e3)
//
//		e1 R (e1 + e2)
//    
//         R
//      e1   +
//         e1 e2
//
//        +
//      R   R
//   e1 e1  e1 e2
//
//
//   (e1 + e2) R (e1 + e2)
//  
//		R
	+		+
  e1 e2   e1  e2

		+
	R        R
  +   e1    +   e2
e1 e2    (e1 e2)

// then you need to distribute again, and you get this 'blowing out' of products

// but if you have regressive product followed by regressive product
   e1 R e2 R e3 

// what do you end up with?

	R
 e1   R  <-- expand this node
    e2  e3

<-- expand the node, then simplify
<-- expand the node, then simplify

what i need is a page of notes that has all these examples in it so I can easily reference them
but this is the idea i forgot, I just assumed that you could simplify right off the rip which doesn't seem
to be the case. we need the addition of expand and simplify



Environment :: struct {
	dimension: int;
	basis_one_elements: [] string;
	n_element: int;
}

// set_grassman_environment_variables(

// need an environment that will have the grassmann info in there
// the symbols, the 

// TEMPORARY!!
dimension: int;
n_element: int;

exterior_product :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( result: K_Element ) {

	// assume the result is zero, change it if it isn't the case
	result: K_Element;
	if lvalue.grade == 0 || rvalue.grade == 0 {
		result.grade 	   			  = lvalue.grade + rvalue.grade; 
		result.coefficient 			  = lvalue.coefficient * rvalue.coefficient;
		result.one_element_components = lvalue.one_element_components | rvalue.one_element_components;
	}
	else
	if (lvalue.one_element_components & rvalue.one_element_components) == 0 { // no one elements in common; implies sum of grades is less than or equal to N
		result.grade 	   			  = lvalue.grade + rvalue.grade;
		result.coefficient 			  = lvalue.coefficient * rvalue.coefficient;
		result.one_element_components = lvalue.one_element_components | rvalue.one_element_components;

		result.coefficient *= anticommutative_coefficient( lvalue, rvalue );
	}

	return result;
}

anticommutative_coefficient :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( coefficient: int ) {

	// This procedure calculates the number of swaps required to reorder the elements into the 'canonical' ordering.
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
	for < i: dimension-1..0 {
		if lvalue.one_element_components & (1 << i) {
			for j: 0..dimension-1 {
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

// maybe make it take the result location as a parameter? that allows you to put in the location
regressive_product :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( result: K_Element ) {

	// assume the result is zero, change it if it isn't the case
	result: K_Element;

	if lvalue.grade == dimension {
		// return the right-operand 
		result.grade 	   			  = rvalue.grade;
		result.coefficient 			  = rvalue.coefficient * lvalue.coefficient;
		result.one_element_components = rvalue.one_element_components;
	}
	else 
	if rvalue.grade == dimension {
		// return the left-operand
		result.grade 				  = lvalue.grade;
		result.coefficient 			  = lvalue.coefficient * rvalue.coefficient;
		result.one_element_components = rvalue.one_element_components; 
	}

	return result;
}

regressive_product_fast :: ( lvalue: *K_Element, rvalue: *K_Element ) -> ( result: *K_Element ) {

	// (lvalue OR rvalue)   == N-element
	// (lvalue AND rvalue)  == element they share in common, could be zero if a scalar
	// (lvalue XOR element) == the elements in the lvalue NOT in the elements in the rvalue
	// use the anticommutative_coefficient procedure to calculate whether the element needs negated or not

	result := New(K_Element);
	result.grade = 0;
	result.coefficient = 0;
	result.one_element_components = 0;

	if (lvalue.one_element_components | rvalue.one_element_components) == n_element {
		result.grade = (lvalue.grade + rvalue.grade) - dimension;
		result.one_element_components = lvalue.one_element_components & rvalue.one_element_components;
	
		rvalue_complement := lvalue.one_element_components ^ result.one_element_components;
		
		result.coefficient *= anticommutative_coefficient( rvalue_complement, rvalue );
	}

	return result;
}

complement :: ( op: Complement_Type, k_element: *K_Element ) -> ( n_k_element: K_Element ) {
	
	n_k_element: K_Element;
	n_k_element.grade 				   = dimension - k_element.grade;
	n_k_element.coefficient 		   = k_element.coefficient;
	n_k_element.one_element_components = n_element ^ k_element.one_element_components;

	// Need to determine whether the (n-k)-element should be negated.
	// The procedure below effectively takes the exterior product between the args and determines the number
	// of swaps required to build the final N element from the product of the (n-k)-element or the k-element
	// or vice versa depending on whether this is a left or right complement.

	if op == .Left {
		n_k_element.coefficient *= anticommutative_coefficient( *n_k_element, k_element );
	} 
	else
	if op == .Right {
		n_k_element.coefficient *= anticommutative_coefficient( k_element, *n_k_element );
	}

	return n_k_element;
}

generate_k_span :: ( element: *K_Element, k: int ) -> ( k_span: [..] K_Span_Element ) {

	// TODO: This doesn't handle k-elements with a coefficient other than 1!
	
	// to generate a k-span you need the 1-span
	one_span: [..] int;
	for bit: 0..dimension-1 {
		if element.one_element_components & (1 << bit) {
			array_add( *one_span, bit );
		}
	}

	k_span: [..] K_Span_Element;
	if k == 1 {
		k_element: K_Span_Element;
		k_element.grade 	  = 1;
		k_element.coefficient = 1;
		for i: 0..one_span.count-1 {
			k_element.one_element_components = (1 << one_span[i]);
			array_add( *k_span, k_element );
		}
	}
	else {
		// build the set of k-elements (k-combinations) from the one-span.
		// this is O(N^3), probably a better way to do this!

		one_element: K_Span_Element;
		one_element.grade 		= 1;
		one_element.coefficient = 1;
	
		for i: 0..one_span.count-1 {

			one_element.one_element_components = (1 << one_span[i]);

			for j: i+1..one_span.count-1 {

				k_element := one_element; // copy one element into the k-element we are building

				num_remaining_one_elements := one_span.count-j;
		
				if num_remaining_one_elements >= (k-1) {	
					s: int = 0;
					while k_element.grade != k {
						k_element.one_element_components |= (1 << one_span[j+s]);
						k_element.grade += 1;
						s += 1;
					}

					array_add( *k_span, k_element );
				}
				else {
					break;
				}
			}
		}
	}

	return k_span;
}

generate_k_cospan :: ( m_element: *K_Element, k_span: [] K_Span_Element ) -> ( k_cospan: [..] K_Cospan_Element ) {

	co_m_element :: ( m_element: *K_Element, k_element: *K_Element ) -> ( m_k_element: K_Element ) {

		// similar to the 'complement' however this is with respect to an m-element instead of the basis n-element.
		
		m_k_element: K_Element;
		m_k_element.grade				   = m_element.grade - k_element.grade;
		m_k_element.coefficient 		   = 1;
		m_k_element.one_element_components = m_element.one_element_components ^ k_element.one_element_components;

		m_k_element.coefficient *= anticommutative_coefficient( k_element, *m_k_element );

		return m_k_element;
	}

	k_cospan: [..] K_Cospan_Element;
	for k_element: k_span {
		m_k_element := co_m_element( m_element, *k_element );
		array_add( *k_cospan, m_k_element );
	}

	return k_cospan;
}
