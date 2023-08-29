#scope_export

Interpreter :: struct {	
	ast: *Ast_Node;
}



eval :: ( interp: *Interpreter ) -> ( result: string ) {

	interp.ast = distribute_grassmann_products( interp.ast );
	interp.ast = apply_common_factor_theorem( interp.ast );
	interp.ast = simplify( interp.ast );

	elements: [..] K_Element_Node;
	
	
	
}


#scope_file
