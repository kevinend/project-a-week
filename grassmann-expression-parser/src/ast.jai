#scope_export 

Node_Kind :: enum {
	
	K_Element; // support for 'origin' as basis, so no longer just k-vectors
	Unary_Op;
	Binary_Op;
	Identifier;
	Literal;
}

Ast_Node :: struct {
	kind: Node_Kind;
}

K_Element :: struct {
	grade: int;
	coefficient: int;
	one_element_components: s64; // bit-vector for the one elements in the space
}

K_Element_Node :: struct {

	#as using _kind: Ast_Node;
	
	using k_element: K_Element;
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
	Regressive_Product_Post_CFT; 
	Interior_Product;
}

Binary_Operator_Node :: struct {

	#as using _kind: Ast_Node;

	op: 	Binary_Op;
	left:  *Ast_Node;
	right: *Ast_Node;
}
