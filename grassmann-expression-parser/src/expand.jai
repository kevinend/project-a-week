#scope_export

// TODO: Identifier resolution!!!

// since we store values as Ast_Nodes we need a way to 'hook in' the identifier values in the symbol table
// then make sure to expand then

expand :: ( node: *Ast_Node ) -> ( subtree: *Ast_Node ) {

	// Distribute the grassmann products over the arithmetic product (sum)
	// 
	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.

	subtree: *Ast_Node;
	if node.kind == .K_Element {
		subtree = clone_node( node );
	}
	else
	if node.kind == .Unary_Op {
		unary_operator := cast(*Unary_Operator_Node)node;
		unary_operator.operand = expand( unary_operator.operand );

		need_to_distribute: bool = false;
		if unary_operator.operand.kind == .Binary_Op {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;
			if binary_operator.op == .Sum {
				need_to_distribute = true;
			}
		}

		if need_to_distribute {
			binary_operator := cast(*Binary_Operator_Node)unary_operator.operand;

			new_\left := new_unary_operator_node( unary_operator.op, clone_node( binary_operator.left ) );
			new_right := new_unary_operator_node( unary_operator.op, clone_node( binary_operator.right ) );

			new_binary_operator := new_binary_operator_node(
				 binary_operator.op
				,expand( new_left )
				,expand( new_right )
			);

			subtree = new_binary_operator;
		}
		else {
			subtree = clone_node( node );
		}
	}
	else
	if node.kind == .Binary_Op {
		
		binary_operator := cast(*Binary_Operator_Node)node;
		binary_operator.left  = expand( binary_operator.left  );
		binary_operator.right = expand( binary_operator.right );

		if binary_operator.op == .Exterior_Product || binary_operator.op == .Regressive_Product {
			
			// left and right are *Ast_Nodes
			// if they turn out to be Binary_Operator_Nodes then these 'early assignments'
			// saves us from introducing new variables if we DO have to convert to operators.

			left  := cast(*Binary_Operator_Node)binary_operator.left;
			right := cast(*Binary_Operator_Node)binary_operator.right;
			
			if left.kind == .Binary_Op && left.op == .Sum {
				// distribute left over right
				// take the argument from the left and distribute it over the right
				grassmann_op := binary_operator.op;
			
				new_left := new_binary_operator_node(
					 grassmann_op
					,clone_node( left.left )
					,clone_node( binary_operator.right )
				);

				new_right := new_binary_operator_node(
					 grassmann_op
					,clone_node( left.right )
					,clone_node( binary_operator.right )
				);

				// to fully expand the subtreess need to recursively call this procedure again
				new_binary_operator := new_binary_operator_node(
					 left.op
					,expand( new_\left )
					,expand( new_right )
				);

				subtree = new_binary_operator;
			}
			else 
			if right.kind == .Binary_Op && right.op == .Sum {
				// distribute right over left
				// take the argument from the right and distribute it over the left
				grassmann_op := binary_operator.op;

				new_\left := new_binary_operator_node(
					 grassmann_op
					,clone_node( binary_operator.left )
					,clone_node( right.left  )
				);

				new_right := new_binary_operator_node(
					 grassmann_op
					,clone_node( binary_operator.left )
					,clone_node( right.right )
				);

				// to fully expand the subtrees need to recursively call this procedure
				new_binary_operator := new_binary_operator_node(
					 right.op
					,expand( new_\left )
					,expand( new_right )
				);

				subtree = new_binary_operator;	

			}
			else {
				subtree = clone_node( node );
			}
		}
		else {
			subtree = clone_node( node );
		}
	}
	else
	if node.kind == .Identifier {
		subtree = clone_node( node );
	}
	else
	if node.kind == .Literal {
		subtree = clone_node( node );
	}

	return subtree;
}

