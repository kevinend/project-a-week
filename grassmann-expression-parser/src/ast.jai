#scope_export 

Node_Kind :: enum {
	
	K_Element; // support for 'origin' as basis, so no longer just k-vectors
	Literal;
	Identifier;
	Unary_Op;
	Binary_Op;
}

Ast_Node :: struct {
	kind: Node_Kind;
}

K_Element_Node :: struct {
	
	#as using _kind: Ast_Node;	
	using _k_element: K_Element;
}

Identifier_Node :: struct {
	
	#as using _kind: Ast_Node;
	name: string;
}

Literal_Node :: struct {

	#as using _kind: Ast_Node;
	value: int;
}

Unary_Op :: enum {
	
	Negation;
	Left_\Complement;
	Right_Complement;
}

Unary_Operator_Node :: struct {
	
	#as using _kind: Ast_Node;

	op: Unary_Op;
	operand: *Ast_Node;
}

Binary_Op :: enum {
	
	Assign;
	Sum;
	Difference;
	Exterior_Product;
	Regressive_Product;
	Interior_Product;
}

Binary_Operator_Node :: struct {

	#as using _kind: Ast_Node;

	op: 	Binary_Op;
	left:  *Ast_Node;
	right: *Ast_Node;
}

clone_node :: ( node: *Ast_Node ) -> ( cloned_node: *Ast_Node ) {

	cloned_node: *Ast_Node = null;
	if node.kind == .K_Element {
		k_element 	:= cast(*K_Element_Node)node;
		cloned_node = new_k_element_node( k_element.coefficient, k_element.basis );
	}
	else
	if node.kind == .Identifier {
		identifier  := cast(*Identifier_Node)node;
		cloned_node = new_identifier_node( identifier.name );
	}
	else
	if node.kind == .Literal {
		literal 	:= cast(*Literal_Node)node;
		cloned_node = new_literal_node( literal.value );
	}
	else
	if node.kind == .Unary_Op {
		unary_operator := cast(*Unary_Operator_Node)node;
		cloned_node    = new_unary_operator_node( unary_operator.op, clone_node( unary_operator.operand ) );
	}
	else
	if node.kind == .Binary_Op {
		binary_operator := cast(*Binary_Operator_Node)node;
		left  := clone_node( binary_operator.left  );
		right := clone_node( binary_operator.right );

		cloned_node = new_binary_operator_node( binary_operator.op, left, right );
	}
	else {
		print( "Unrecognized node kind found when cloning nodes\n" );
	}
	
	return cloned_node;	
}

new_k_element_node :: ( coefficient: int, basis: s64 ) -> ( node: *K_Element_Node ) {

	// node := New(K_Element_Node);

	node: *K_Element_Node = talloc( size_of(K_Element_Node) );
	assert( node != null );

	node.kind  		 = Node_Kind.K_Element;
	node.coefficient = coefficient;
	node.basis 		 = basis;

	return node;
}

new_literal_node :: ( value: int ) -> ( node: *Literal_Node ) {

	// node := New(Literal_Node);

	node: *Literal_Node = talloc( size_of(Literal_Node) );
	assert( node != null );
	
	node.kind = Node_Kind.Literal;
	node.value = value;

	return node;
}

new_identifier_node :: ( name: string ) -> ( node: *Identifier_Node ) {

	// node := New(Identifier_Node);

	node: *Identifier_Node = talloc( size_of(Identifier_Node) );
	assert( node != null );

	node.kind = .Identifier;
	node.name = name; 		// alias the allocated identifier name from the lexer; set to temporary storage and wiped on each iteration

	return node;
}

new_unary_operator_node :: ( op: Unary_Op, operand: *Ast_Node ) -> ( node: *Unary_Operator_Node ) {

	// node := New(Unary_Operator_Node);

	node: *Unary_Operator_Node = talloc( size_of(Unary_Operator_Node) );
	assert( node != null );

	node.kind 	  = .Unary_Op;
	node.op 	  = op;
	node.operand  = operand;

	return node;
}

new_binary_operator_node :: ( op: Binary_Op, left: *Ast_Node, right: *Ast_Node ) -> ( node: *Binary_Operator_Node ) {

	// node := New(Binary_Operator_Node);
	
	node: *Binary_Operator_Node = talloc( size_of(Binary_Operator_Node) );
	assert( node != null );

	node.kind  = .Binary_Op;
	node.op    = op;
	node.left  = left;
	node.right = right;

	return node;
}
