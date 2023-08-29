#scope_export

expand :: ( ast: *Ast_Node ) -> ( expanded_ast: *Ast_Node ) {

	ast = distribute_grassmann_products( ast );
	ast = apply_common_factor_theorem( ast );

	return ast;
}

#scope_file

node_is_k_element :: inline ( node: *Ast_Node ) -> ( yes_no: bool ) {
	return node.kind == .K_Element;
}

node_is_unary_operator :: inline ( node: *Ast_Node ) -> ( yes_no: bool ) {
	return node.kind == .Unary_Operator;
}

node_is_binary_operator :: inline ( node: *Ast_Node ) -> ( yes_no: bool ) {
	return node.kind == .Binary_Operator;
}

node_is_identifier :: inline ( node: *Ast_Node ) -> ( yes_no: bool ) {
	return node.kind == .Identifier;
}

node_is_literal :: inline ( node: *Ast_Node ) -> ( yes_no: bool ) {
	return node.kind == .Literal;
}

operator_is_grassmann_product :: ( op: Binary_Operation ) -> ( yes_no: bool ) {
	if op == .Exterior_Product || op == .Regressive_Product || op == .Interior_Product {
		return true;
	}
	else {
		return false;
	}
}

clone_node :: ( node: *Ast_Node ) -> ( cloned_node: *Ast_Node ) {

	cloned_node: *Ast_Node = null;
	if node_is_k_element( node ) {
		k_element 	:= cast(*K_Element_Node)node;
		cloned_node := new_k_element_node( k_element.grade, k_element.coefficient, k_element.one_element_components );
	}
	else
	if node_is_identifier( node ) {
		identifier  := cast(*Identifier_Node)node;
		cloned_node := new_identifier_node( identifier.name );
	}
	else
	if node_is_literal( node ) {
		literal 	:= cast(*Literal_Node)node;
		cloned_node := new_literal_node( literal.value );
	}
	else
	if node_is_unary_operator( node ) {
		unary_operator := cast(*Unary_Operator_Node)node;
		cloned_node    := new_unary_operator_node( unary_operator.operation, clone_node( unary_operator.operand ) );
	}
	else
	if node_is_binary_operator( node ) {
		binary_operator := cast(*Binary_Operator_Node)node;
		cloned_node     := new_binary_operator_node(
			 binary_operator.operation
			,clone_node( binary_operator.left\_operand )
			,clone_node( binary_operator.right_operand )
		);
	}
	else {
		print( "Unrecognized node kind found when cloning nodes\n" );
	}
	
	return cloned_node;	
}

distribute_grassmann_products :: ( node: *Ast_Node ) -> ( ast: *Ast_Node ) {

	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.

	subtree: *Ast_Node;
	if node_is_k_element( node ) {
		subtree = clone_node( node );	
	}
	else 
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
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
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
			grassmann_operator       := binary_operator.operation;
			distribute_over_operator := left_operator.operation;

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
				 distribute_over_operator // sum or difference
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else
		if distribute_over_right_operand {
			// distribute the argument on the left over the argument on the right
			grassmann_operator    	 := binary_operator.operation;
			distribute_over_operator := right_operator.operation;

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
				 distribute_over_operator // sum or difference
				,distribute_grassmann_products( new_left\_node )
				,distribute_grassmann_products( new_right_node )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}

	return subtree;
}

simplify :: ( node: *Ast_Node ) -> ( result: *Ast_Node ) {

	result: *Ast_Node;
	if node_is_k_element( node ) {
		result = clone_node( node );
	}
	else
	if node_is_unary_operator( node ) {
		unary_operator := cast(*Unary_Operator)node;
		unary_operator.operand = simplify( unary_operator.operand );

		assert( unary_operator.operand.kind == .K_Element );

		operand := cast(*K_Element_Node)unary_operator.operand;
		
		if unary_operator.operation == .Left_Complement {
			result = complement( .Left, operand );
		}
		else
		if unary_operator.operation == .Right_Complement {
			result = complement( .Right, operand );
		}
		else
		if unary_operator.operation == .Negation {
			operand.coefficient *= -1;
			result = clone_node( operand );
		}
	}
	else {
		// identifiers and literals not handled here
		binary_operator := cast(*Binary_Operator_Node)node;
	
		binary_operator.left\_operand = simplify( binary_operator.left\_operand );
		binary_operator.right_operand = simplify( binary_operator.right_operand );

		if node_is_k_element( binary_operator.left\_operand ) && node_is_k_element( binary_operator.right_operand ) {
			left  := cast(*K_Element_Node)binary_operator.left\_operand;
			right := cast(*K_Element_Node)binary_operator.right_operand;

			operation := binary_operator.operation;
			if operation == .Exterior_Product {
				result = exterior_product( left, right );
			}
			else
			if operation == .Regressive_Product {
				result = regressive_product( left, right ); 
			} 

			// sums and differences are handled in a separate pass
		}
	}
}

simplify_sums :: ( node: *Ast_Node ) -> ( result: *Ast_Node ) {

/*
	 once we run simplify over the entire tree we will be left with a bunch of sums
	 this procedure basically implements a poor man's hashtable to collect sums across
	 nodes in the tree.
	
	 post-simplify we could end up with trees like
	    +
	  e2  +
	    e1 e2
	
	 we want the final result to be 2e2 + e1 (in this order, left subtree first with sums from the right, then anything from the right subtree)
	
	 so we know that the for a given dimension there are 2^n elements (including one element representing all scalars)
	 and given how we defined a K-Element the 'one-element-components' as bit-fields will cover the entire 2^n range.
	
	 so we pre-allocate space to store any possible k-element given the current dimension (needs changed if dimension changes at runtime)
	 and we traverse the tree and if we find a k-element we go to it's place in the preallocated memory and do the sum
	
	we also want to preserve the order that we found the elements so we use a single byte to track the order of the indices in the
	'sums' array in the order we found the element (this 'display_order' and we don't store duplicates, this is a lot of checking for
 	to make sure there are no duplicates for dimensions >= 4 but we only store a single byte so everything should be in cache.
	still want to think of a better way to do this but it will work for now
	
	G(R2) = 4 total possible k-elements
	1    = 0
	e1   = 1
	e2   = 2
	e12 '= 3

	G(R3) = 8 total possible k-elements
	1       = 0
	e1		= 1
	e2		= 2
	e3		= 4
	e12     = 3
	e13     = 5
	e23     = 6
	e123    = 7
	
	G(R4) = 16
	1  		= 0
	e1 		= 1
	e2 		= 2
	e3 		= 4
	e4 		= 8
	e12 	= 3
	e13 	= 5
	e23 	= 6
	e14 	= 9
	e24 	= 10
	e34 	= 12
	e123 	= 7 
	e124	= 11
	e134    = 13
	e234	= 14
	e1234 	= 15
*/
 
	helper :: ( node: *Ast_Node, sums: [] *K_Element, order: [] u8, num_elements: *int ) {
		
		if node_is_k_element( node ) {
			
			k_element := cast(*K_Element_Node)node;
			index     := k_element.one_element_components;
	
			sums[index].coefficient 				+= k_element.coefficient;
			sums[index].basis_one_element_components = k_element.basis_one_element_components; // wasteful, implicit from the index but makes final result easier to compute

			if sums[index].grade == 0 {
				sums[index].grade   = k_element.grade;
				order[num_elements] = index;
				num_elements += 1;
			}

			return;
		}
		
		if node_is_binary_operator( node ) {
			binary_operator: cast(*Binary_Operator_Node)node;

			assert( binary_operator.operation == .Sum );

			if node_is_binary_operator( binary_operator.left\_operand ) {
				helper(	binary_operator.left\_operand ); 
			}
			

			if node_is_binary_operator( binary_operator.right_operand ) {
				helper( binary_operator.right_operand );
			} 
		}
	}

	sums: [16] *K_Element; // NEED TO ALLOCATE THESE
	for i: 0..15 {
		sums[i].coefficient = 0;
		sums[i].grade       = 0; // important! don't get rid of this without reviewing!
	}

	order: [16] u8;
	num_elements: int = 0;

	helper( node, sums, order, *num_elements );

	// now you have the simplified sums across the tree in a list
	// turn this into another tree which is the final result of the computation

	subtree: *Ast_Node;
	if num_elements == 1 {
		k_element := clone_node( sums[order[0]] );
		subtree    = k_element;
	}
	else {
		root = new_binary_operator_node( .Sum, null, null );

		i: int = 0;
		current := root;
		while i < num_elements-2 {
			
			k_element := clone_node( sums[order[i]] );
			current.left_operand = k_element;

			next := new_binary_operator_node( .Sum, null, null );
			
			current.right_operand = next;
			current = next;

			i += 1;
		}
	

		k_element := clone_node( sums[order[i]] );
		current.left\_operand = k_element;

		k_element := clone_node( sums[order[i+1] );
		current.right_operand = k_element;

		subtree = root;
	}

	return subtree;	
}

apply_common_factor_theorem :: ( node: *Ast_Node ) -> ( ast: *Ast_Node ) {

	subtree: *Ast_Node;
	if !node_is_binary_operator( node ) {
		subtree = clone_node( node );
	}
	else {
		// Determine whether we need to apply the common factor theorem to any regressive products.
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.operation == .Regressive_Product {
			left  := cast(*K_Vector_Node)simplify( binary_operator.left\_operand );
			right := cast(*K_Vector_Node)simplify( binary_operator.right_operand );

			if (left.grade + right.grade) < dimension {
				// regressive product is zero, don't bother expanding
				subtree = clone_node( node );
			}
			else {
				// generate the k-span and k-cospan of the right node.
				// k represents the grade of the element the left and right share in common.
				// Example:
				//	 N = 4
				//   A = e1^e3
				//   B = e2^e3^e4
				//   k = (Grade(A) + Grade(B)) - N == (2+3) - 4 == 5-4 = 1
				// 1-span:	 [e2,e3,e4]
				// 1-cospan: [e3e4, -e2e4, e2e3]

				k := (left.grade + right.grade) - dimension;
				k_span:	  [..] *K_Vector_Node = generate_k_span\ ( right, k );
				k_cospan: [..] *K_Vector_Node = generate_k_cospan( right, k_span );

				// build the regressive product subtrees from the k-span and k-cospan elements.
				// Example: using the span and cospan from the above example
				// regressive_product_subtrees = [subtree0, subtree1, subtree2, ...]
				//   where subtree0 =		R
				//						^     k-span[0]
				//				 	  L  k-cospan[0]

				regressive_product_subtrees: [..] *Binary_Operator_Node;
				for i: 0..k_span.count-1 {
				
					exterior_product_node := New(Binary_Operator_Node);
					init_binary_operator( 
						 exterior_product_node
						,.Exterior_Product
						,clone_node( left )
						,k_cospan[i] 
					);

					regressive_product_node := New(Binary_Operator_Node);
					init_binary_operator(
						 regressive_product_node
						,.Regressive_Product
						,exterior_product_node
						,k_span[i]
					);

					array_add( *regressive_product_subtrees, regressive_product_node );
				}

				// generate the new root node with the sums of the regressive product subtrees created above
				// Example: convert the subtrees into a tree 'sum', provided there is more than one subtree
				// regressive_product_subtrees: [subtree0, subtree1, subtree2, subtree3]
				//      		+
				// 		subtree0    +
				//      	subtree1 	+
				//        		subtree2 subtree3


				if regressive_product_subtrees.count == 1 {
					subtree = regressive_product_subtrees[0];
				}
				else {

					root := New(Binary_Operator_Node);
					init_binary_operator( root, .Sum, null, null );
					
					i: int = 0;
					current := root;
					while i < regressive_product_subtrees.count-2 { 
						current.left_operand = regressive_product_subtrees[i];
						
						next := New(Binary_Operator_Node);
						init_binary_operator( next, .Sum, null, null );

						current.right_operand = next;
						current = next;

						i += 1;
					}

					// the final sum needs handled outside the loop
					current.left\_operand = regressive_product_subtrees[i];
					current.right_operand = regressive_product_subtrees[i+1];

					subtree = root;
				}
			}
		}
		else {
			// binary operator node is not a regressive product, just clone the node and return it
			subtree = clone_node( node );
		}
	}

	return subtree;
}

