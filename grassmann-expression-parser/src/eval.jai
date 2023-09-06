#scope_export



// [e1,e2,e3,e12,e13,e23...]
// can map into this on the basis, space so you evaluate the grassmann products then 
// but what if you have something like
//  e1 ^ (e2 ^ e3)
//   ^
// e1 ^
//    e2 e3
// you don't want to eval and put in e2e3 then e1, or e2e3 and then e1e2e3
// so exterior and regressive products return + and then those sums use the values
// it's just the way she has to go unfortunately
// man it would be cool to just work off the table...
// 
// maybe we thread the product through the recursive calls?



// i had this thought that maybe we can use the table for everything...
// like we iterate through there, instead of creating new memory we just go through
// at the end you are left with sums
// so you maintain the order and when you resolve an exterior product you just add it to the sum

Result :: struct {
	coefficient: int = 0;
	updated: bool;
}

result:  	  [64] Result;
result_order: [64] u8;

evaluate :: ( node: *Ast_Node, env: *Environment ) {

	// memset to zero
	for i: 0..result.count-1 {
		result[i].coefficient = 0;
		result[i].updated     = false;

		result_order[i]  = 0;
	}

	
	// This application accepts the following forms of expressions.
	// (1) Ident = Expression;
	// (2) Ident;
	// (3) Expression;
	//
	// In case (1) we eval the expression and store the value (simplified ast) in a symbol table
	// In case (2) we traverse the ast for the identifier and print out the resulting expression
	// In case (3) we traverse the ast, simplify it as much as possible and print the result

	result: *Ast_Node = null;
	if node.kind == .Identifier { // or we could notate it and just move on, like if there is a token and a semicolon then don't substitute
		print( "We need to print the identifier\n" );
		result = null;
	}
	else
	if node.kind == .Binary_Op {
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.op == .Assign {
			// this is eval and assign
			result = eval( binary_operator.right, env );
			print_ast( result );

			// once we have a symbol table we can add stuff in there
			// just copy that over out of the temp memory into the symbol table
		}
		else {
			// this is just eval the inline expression
			// the parser is not set up for this yet!
		}
	}
}

eval :: ( node: *Ast_Node, env: *Environment ) -> ( result: *Ast_Node ) {

	result: *Ast_Node;
	if node.kind == .K_Element || node.kind == .Literal {
		result = clone_node( node );
	}
	else
	if node.kind == .Unary_Op {
		unary_operator := cast(*Unary_Operator_Node)node;
		operand := eval( unary_operator.operand, env );
		result   = eval_unary_operator( unary_operator.op, operand, env );
	}
	else 
	if node.kind == .Binary_Op {
		// might want to check for errors or something after left and right???
		binary_operator := cast(*Binary_Operator_Node)node;
		left  := eval( binary_operator.left,  env );
		right := eval( binary_operator.right, env );

		if binary_operator.op == .Exterior_Product || binary_operator.op == .Regressive_Product {
			result = eval_binary_operator( binary_operator.op, left, right );
		}
		else 
		if binary_operator.op == .Sum {
			if left.kind == .K_Element {
				
			}
		}


		
/*
		if left.kind == .K_Element && right.kind == .K_Element {
			result = eval_binary_operator( binary_operator.op, cast(*K_Element_Node)left, cast(*K_Element_Node)right, env );
		}
		else {
			result = clone_node( node );
		}
*/
	}

	return result;
}

#scope_file

Complement_Type :: enum u8 {
	Left;
	Right;
}

eval_unary_operator :: ( op: Unary_Op, operand: *Ast_Node, env: *Environment ) -> ( result: *Ast_Node ) {

	result: *Ast_Node;

	assert( operand.kind == .K_Element );
	k_element := cast(*K_Element_Node)operand;

	if op == {
		case .Negation; {
			k_element.coefficient *= -1;
			result = new_k_element_node( k_element.coefficient, k_element.basis );
		}
		case .Left_Complement; {
			result = eval_complement( .Left, k_element, env.n_element, env.dimension );
		}
		case .Right_Complement; {
			result = eval_complement( .Right, k_element, env.n_element, env.dimension );
		}
	}

	return result;
}

eval_complement :: ( type: Complement_Type, k_element: *K_Element_Node, n_element: s64, dimension: int ) -> ( n_k_element: *K_Element_Node ) {

	n_k_element := new_k_element_node( 
		 coefficient = k_element.coefficient
		,basis 	     = n_element ^ k_element.basis 
	);

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

anticommutative_coefficient :: ( lvalue: *K_Element_Node, rvalue: *K_Element_Node, dimension: int ) -> ( coefficient: int ) {

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

	temp: int = rvalue.basis;
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

eval_binary_operator :: ( op: Binary_Op, lvalue: *K_Element_Node, rvalue: *K_Element_Node, env: *Environment ) -> ( result: *Ast_Node ) {
	
	result: *Ast_Node;

	if op == {
		case .Sum; {
			// sums are handled in a second eval pass
			result = new_binary_operator_node( op, lvalue, rvalue );
		}
		case .Exterior_Product; {
			result = eval_exterior_product( lvalue, rvalue );
		}
		case .Regressive_Product; {
			result = eval_regressive_product( lvalue, rvalue, env.dimension );
		}
	}

	return result;
}



eval_exterior_product :: ( lvalue: *K_Element_Node, rvalue: *K_Element_Node ) -> ( result: *K_Element_Node ) {

	result := new_k_element_node( coefficient = 0, basis = 0 );

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

eval_regressive_product :: ( lvalue: *K_Element_Node, rvalue: *K_Element_Node, n_element: s64 ) -> ( result: *K_Element_Node ) {

	result := new_k_element_node( coefficient = 0, basis = 0 );
	
	if (lvalue.basis | rvalue.basis) == n_element {
		result.coefficient = lvalue.coefficient * rvalue.coefficient;
		result.basis       = lvalue.basis & rvalue.basis;

		// not sure about anticommutativity here!!!
	}

	return result;
}

// result table, iterate through there and create the final sums...
// that might be a way to do it, all calculations end up in there
// so once you hit a value it doesn't matter
// let's try that so there isn't a separate, sum thingy
eval_sums :: ( node: *Ast_Node ) -> ( result: *Ast_Node ) {

	if node.kind == .K_Element {
	}
	
	if node.kind == .Binary_Op {
		// since we resolved all grassmann products and unary operators then this must be a sum!
		binary_operator := cast(*Binary_Operator_Node)node;
		assert( binary_operator.op == .Sum );
		
		left  := eval_sum( binary_operator.left  );
		right := eval_sum( binary_operator.right );
	}
}
