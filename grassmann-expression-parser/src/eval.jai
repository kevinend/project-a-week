#scope_export

Symbol :: struct {
	name:   	 string;
	ast:   		*Ast_Node;
	expression:  string;
}

MAX_NUM_SYMBOLS :: 8;

Interpreter :: struct {
	stack: 		  Stack(K_Element);  				   // used to execute grassmann products
	multielement: [MAX_NUM_BASIS_ELEMENTS] K_Element;  // final output from expression evaluation
	num_terms:    int;           				   	   // number of terms in the multielement

	symbols: [MAX_NUM_SYMBOLS] Symbol;
	num_symbols: int;
}

print_symbol_table :: ( interp: *Interpreter ) {

	print( "Symbols belonging to symbol table:\n" );
	for i: 0..interp.num_symbols-1 {
		print( "%\n", interp.symbols[i].name );	
	}
	print( "\n" );

	print( "Name = Value\n" );
	for i: 0..interp.num_symbols-1 {
		print( "% = %\n", interp.symbols[i].name, interp.symbols[i].expression );
	}
	print( "\n" );

	return;
}

reset_interpreter :: ( interp: *Interpreter ) {

	if interp.stack.entries.count == 0 { // unitialized
		interp.stack = stack_init( 50, K_Element ); // TODO: make this reasonable
	}

	// should be memset!
	for i: 0..interp.multielement.count-1 {
		interp.multielement[i].coefficient =  0;
		interp.multielement[i].basis 	   = -1;
	}

	interp.num_terms = 0;
}

lookup_symbol :: ( name: string, interp: *Interpreter ) -> ( value: *Ast_Node, success: bool ) {
	
	value: *Ast_Node = null;
	success: bool = false;
	for i: 0..interp.num_symbols-1 {
		if interp.symbols[i].name == name {
			value   = interp.symbols[i].ast;
			success = true;
			break;
		}
	}

	return value, success;
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
	
	if node.kind == .Identifier {
		ident := cast(*Identifier_Node)node;
		value, success := lookup_symbol( ident.name, interp );
		if success {
			print_ast( value, "Identifier:" );
		}
		else {
			print( "null" ); // i guess it technically isn't an error
		}
	}
	else
	if node.kind == .Binary_Op {
		binary_operator := cast(*Binary_Operator_Node)node;
		if binary_operator.op == .Assign {
			assert( binary_operator.left.kind == .Identifier );
			
			ident 	   := cast(*Identifier_Node)binary_operator.left;
			expression := binary_operator.right;

			// Eval and Assign
			eval( expression, env, interp ); 		// should this be called eval_grassmann_products_and_unary_operators
			build_multielement( interp, env );      // should this be called eval_sums?
			ast  := convert_multielement_to_ast( interp );
			expr := convert_multielement_to_expression( interp, env );
			update_symbol_table( ident.name, ast, expr, interp );

			print( "Interp.multielement == %\n", interp.multielement );
			print( "Number of multielements == %\n", interp.num_terms );
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

convert_multielement_to_ast :: ( interp: *Interpreter ) -> ( ast: *Ast_Node ) {

	ast: *Ast_Node = null;
	if interp.num_terms == 1 {
		ast = new_k_element_node( coefficient = interp.multielement[0].coefficient, interp.multielement[0].basis );
	}
	else {
		root := new_binary_operator_node( .Sum, null, null );

		i: int = 0;
		current := root;
		while i < interp.num_terms-2 {
			defer i += 1;
			current.left = new_k_element_node( coefficient = interp.multielement[i].coefficient, interp.multielement[i].basis );

			next := new_binary_operator_node( .Sum, null, null );
			current.right = next;
			current = next;
		}

		// the final sum needs handled outside the loop
		current.left  = new_k_element_node( coefficient = interp.multielement[i].coefficient, interp.multielement[i].basis );
		current.right = new_k_element_node( coefficient = interp.multielement[i+1].coefficient, interp.multielement[i+1].basis );

		ast = root;
	}

	// print( "PRINTING AST NODE FOR THE SYMBOL CONSTRUCTED FROM THE MULTIELEMENT\n" );
	// print_ast( subtree );

	return ast;	
}

convert_multielement_to_expression :: ( interp: *Interpreter, env: *Environment ) -> ( expression: string ) {

	// Grade 0 elements have an empty string as their 'symbol'
	// When printing a grade 0 the coefficient will only be reflected

	builder: String_Builder;
	basis:       s64;
	coefficient: string;

	i := 0;
	while i < interp.num_terms-1 {
		defer i += 1;
		basis       = interp.multielement[i].basis;
		coefficient = tprint( "%", interp.multielement[i].coefficient );

		append( *builder, coefficient );
		append( *builder, env.basis_elements[basis].symbol );
		append( *builder, " + " );	
	}
	basis 		 = interp.multielement[i].basis;
	coefficient  = tprint( "%", interp.multielement[i].coefficient );
	append( *builder, coefficient );
	append( *builder, env.basis_elements[basis].symbol );

	expression: string = builder_to_string( *builder );

	return expression;
}

update_symbol_table :: ( name: string, ast: *Ast_Node, expression: string, interp: *Interpreter ) {

	// if the symbol table gets 'big' for whatever that means we should make this a hashtable
	i: int = 0;
	symbol_index: int = -1;
	while i < interp.num_symbols {
		defer i += 1;
		if interp.symbols[i].name == name {
			break;
		}
	}

	if i < interp.num_symbols {
		// found an existing symbol, update it
		// TODO: THIS SHOULD FREE THE OLD AST'S MEMORY!!!
		interp.symbols[i].ast = ast;
		interp.symbols[i].expression = expression;
	}
	else {
		// did not find an existing symbol, add it provided we have enough space
		assert( i < MAX_NUM_SYMBOLS );
		interp.symbols[i].name       = name;
		interp.symbols[i].ast  		 = ast;
		interp.symbols[i].expression = expression;

		interp.num_symbols += 1;
	}

	return;
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
	for i: 0..env.num_basis_elements-1 {
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
