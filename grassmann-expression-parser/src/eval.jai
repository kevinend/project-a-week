#scope_export

Interpreter :: struct {
	stack: 		  Stack(K_Element);  				   // used to execute grassmann products
	multielement: [MAX_NUM_BASIS_ELEMENTS] K_Element;  // final output from expression evaluation
	num_terms:    int;               				   // number of terms in the multielement
}

reset_interpreter :: ( interp: *Interpreter ) {

	if interp.stack.entries.count == 0 { // unitialized
		interp.stack = stack_init( 50, K_Element );
	}

	// should be memset!
	for i: 0..interp.multielement.count-1 {
		interp.multielement[i].coefficient =  0;
		interp.multielement[i].basis 	   = -1;
	}

	interp.num_terms = 0;
}

evaluate :: ( node: *Ast_Node, env: *Environment, interp: *Interpreter ) {

	// This application accepts the following forms of expressions.
	// (1) Ident = Expression;
	// (2) Ident;
	// (3) Expression;
	//
	// In case (1) we eval the expression and store the value (simplified ast) in a symbol table
	// In case (2) we traverse the ast for the identifier and print out the resulting expression
	// In case (3) we traverse the ast, simplify it as much as possible and print the result


	reset_interpreter( interp );
	
	result: *Ast_Node = null;
	if node.kind == .Identifier { // or we could notate it and just move on, like if there is a token and a semicolon then don't substitute
		print( "We need to print the identifier\n" );
		result = null;
	}
	else
	if node.kind == .Binary_Op {
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.op == .Assign {
			// Eval and Assign
			eval( binary_operator.right, env, interp ); // should this be called eval_grassmann_products_and_unary_operators
			build_multielement( interp, env );

			print( "Interp.multielement == %\n", interp.multielement );
			print( "Number of multielements == %\n", interp.num_terms );

			// so you want to iterate over this and generate an AST that we stick in the table

			// once we have a symbol table we can add stuff in there
		}
		else {
			// Eval the inline expression
			// The parser is not set up for this yet!
		}
	}
}

eval :: ( node: *Ast_Node, env: *Environment, interp: *Interpreter ) {

	success: bool;
	if node.kind == .K_Element {
		k_element_node := cast(*K_Element_Node)node;
		success = stack_push( *interp.stack, k_element_node._k_element );
	}
	// DOESN"T HANDLE LITERALS LIKE NUMBERS BUT THESE SHOULD BE K-ELEMENTS
	else
	if node.kind == .Unary_Op {
		unary_operator := cast(*Unary_Operator_Node)node;
		eval( unary_operator.operand, env, interp );

		arg: K_Element;
		arg, success = stack_pop( *interp.stack );
		result := eval_unary_operator( unary_operator.op, arg, env, interp );
	}
	else 
	if node.kind == .Binary_Op {
		// might want to check for errors or something after left and right???
		binary_operator := cast(*Binary_Operator_Node)node;
		eval( binary_operator.left,  env, interp );
		eval( binary_operator.right, env, interp );

		if binary_operator.op == .Exterior_Product || binary_operator.op == .Regressive_Product {
			left:  K_Element;
			right: K_Element;

			right, success = stack_pop( *interp.stack );
			left,  success = stack_pop( *interp.stack );
			eval_grassmann_product( binary_operator.op, left, right, env, interp );
		}
		// intentionally excluding .Sum; handled using 'build_multielement'
	}
}

#scope_file

Complement_Type :: enum u8 {
	Left;
	Right;
}

eval_unary_operator :: ( op: Unary_Op, arg: K_Element, env: *Environment, interp: *Interpreter ) {

	result: K_Element;
	if op == {
		case .Negation; {
			result = arg;
			result.coefficient *= -1;
		}
		case .Left_Complement; {
			result = eval_complement( .Left, arg, env.n_element, env.dimension );
		}
		case .Right_Complement; {
			result = eval_complement( .Right, arg, env.n_element, env.dimension );
		}
	}

	success := stack_push( *interp.stack, result );
}

eval_complement :: ( type: Complement_Type, k_element: K_Element, n_element: s64, dimension: int ) -> ( n_k_element: K_Element ) {

	n_k_element: K_Element;
	n_k_element.coefficient = k_element.coefficient;
	n_k_element.basis       = n_element ^ k_element.basis;

	// Need to determine whether the (n-k)-element should be negated.
	// The procedure below effectively takes the exterior product between the args and determines the number
	// of swaps required to build the final N element from the product of the (n-k)-element or the k-element
	// or vice versa depending on whether this is a left or right complement.

	if type == .Left {
		n_k_element.coefficient *= anticommutative_coefficient( n_k_element, k_element, dimension );
	}
	else
	if type == .Right {
		n_k_element.coefficient *= anticommutative_coefficient( k_element, n_k_element, dimension );
	}

	return n_k_element;
}

anticommutative_coefficient :: ( lvalue: K_Element, rvalue: K_Element, dimension: int ) -> ( coefficient: int ) {

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

	temp := rvalue.basis;
	num_swaps: int = 0;
	for < i: dimension-1..0 {
		if lvalue.basis & (1 << i) {
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

eval_grassmann_product :: ( op: Binary_Op, lvalue: K_Element, rvalue: K_Element, env: *Environment, interp: *Interpreter ) {
	
	result: K_Element;
	if op == {
		case .Exterior_Product; {
			result = eval_exterior_product( lvalue, rvalue );
		}
		case .Regressive_Product; {
			result = eval_regressive_product( lvalue, rvalue, env.dimension );
		}
	}

	success := stack_push( *interp.stack, result );
}



eval_exterior_product :: ( lvalue: K_Element, rvalue: K_Element ) -> ( result: K_Element ) {

	result := K_Element.{ coefficient = 0, basis = 0 };

	if lvalue.basis == 0 || rvalue.basis == 0 { // basis = 0 means grade-0 (scalar) element
		result.coefficient  = lvalue.coefficient * rvalue.coefficient;
		result.basis 		= lvalue.basis | rvalue.basis;
	}
	else
	if (lvalue.basis & rvalue.basis) == 0 { // no one elements in common; implies sum of grades is less than or equal to N	
		result.coefficient  = lvalue.coefficient * rvalue.coefficient;
		result.basis 		= lvalue.basis | rvalue.basis;
	}

	return result;
}

eval_regressive_product :: ( lvalue: K_Element, rvalue: K_Element, n_element: s64 ) -> ( result: K_Element ) {

	result := K_Element.{ coefficient = 0, basis = 0 };
	
	if (lvalue.basis | rvalue.basis) == n_element {
		result.coefficient = lvalue.coefficient * rvalue.coefficient;
		result.basis       = lvalue.basis & rvalue.basis;

		// not sure about anticommutativity here!!!
	}

	return result;
}

build_multielement :: ( interp: *Interpreter, env: *Environment ) {

	// This procedure builds a multielement within the interpreter.
	//
	// Once all the sums in the tree are evaluated our interp.multielement will have a list of 
	// integers representing the simplified basis elements that are part of the multielement.
	//
	// Then the application can decide whether that needs converted into an AST for injection into
	// another expression on a subsequent application loop or converted to a string to present to the user.

	// walk the stack and build the multielement in DISPLAY order by using the basis_elements lookup table.
	// the multielement will be sparse after this operation however it will contain the correct sums for each
	// basis element that was part of the original ast.

	while !stack_is_empty( *interp.stack ) {
		element, success := stack_pop( *interp.stack );
		display_index := env.basis_elements[element.basis].display_order;

		interp.multielement[display_index].coefficient += element.coefficient;
		interp.multielement[display_index].basis        = element.basis; // use this as an implicit flag to indicate this index was updated
	}

	// filter/pack the multielement to only include non-empty indices
	// [s1e1, ... s2e1^e2 ... s3e1^e2^e3] will be condensed down to [s1e2,s2e1^e2,s3e1^e2^e3]

	num_scalar_terms: int = 0;
	end: int = 0;
	for i: 0..63 { // this needs changed to the total number of basis elements, should just be part of the environment or something
		if interp.multielement[i].basis >= 0 {
			if interp.multielement[i].basis == 0 {
				num_scalar_terms += 1;
			}
			interp.multielement[end] = interp.multielement[i];
			interp.num_terms += 1;
			end += 1;	
		}
	}

	if num_scalar_terms > 0 {
		interp.num_terms -= (num_scalar_terms - 1); // count scalars a single time
	}

	return;
}
