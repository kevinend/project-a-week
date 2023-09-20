#scope_export

// Not caching the AST or the string expression representation of a multielement
// Both are generated in temporary storage per loop when resolved which is definitely computationally wasteful.
// Don't think it is worth dealing with caching the data structures right now.

Symbol :: struct {
	name:  string;
	value: Multi_Element;
}

Symbol_Table :: struct {

	MAX_NUM_SYMBOLS 	 :: 8;
	SYMBOL_NAME_MAX_SIZE :: 8;
	
	symbols: [MAX_NUM_SYMBOLS] Symbol;
	num_symbols: int;

	names:  [MAX_NUM_SYMBOLS * SYMBOL_NAME_MAX_SIZE]   u8;
	values: [MAX_NUM_SYMBOLS * MAX_NUM_BASIS_ELEMENTS] K_Element;
}

Interpreter :: struct {
	is_initialized: 			bool;
	stack: 		  	   			Stack(K_Element);  				     // used to execute grassmann products
	multielement_builder: 		[MAX_NUM_BASIS_ELEMENTS] K_Element;  // temporary reusable buffer for generating multielements
	num_terms_in_multielement:  int;           				   	     // number of terms in the multielement

	using symbol_table: Symbol_Table;
}

pointer_offset :: inline ( base: *$T, offset: s64 ) -> *void {
	return cast(*void)(cast(*u8)base + offset);
}

print_symbol_table :: ( symbol_table: *Symbol_Table, basis_elements: [] Basis_Element ) {

	print( "Symbol Table Entries:\n" );
	for i: 0..symbol_table.num_symbols-1 {
		expression := multielement_to_expression( symbol_table.symbols[i].value, basis_elements );
		print( "% = %\n", symbol_table.symbols[i].name, expression );
	}
	print( "\n" );

	return;
}

reset_interpreter :: ( interp: *Interpreter ) {

	if !interp.is_initialized {
		interp.stack = stack_init( 50, K_Element ); // TODO: make this reasonable
		for i: 0..Symbol_Table.MAX_NUM_SYMBOLS-1 {
			interp.symbols[i].name.data  = pointer_offset( base = *interp.symbol_table.names[0],  offset = (Symbol_Table.SYMBOL_NAME_MAX_SIZE * i) );
			interp.symbols[i].value.data = pointer_offset( base = *interp.symbol_table.values[0], offset = (MAX_NUM_BASIS_ELEMENTS * size_of(K_Element) * i));
		}
		interp.is_initialized = true;
	}

	// should be memset!
	for i: 0..MAX_NUM_BASIS_ELEMENTS-1 {
		interp.multielement_builder[i].coefficient =  0;
		interp.multielement_builder[i].basis 	   = -1;
	}

	interp.num_terms_in_multielement = 0;
}

lookup_symbol :: ( name: string, symbol_table: *Symbol_Table ) -> ( value: Multi_Element, success: bool ) {
	
	value: Multi_Element;
	success: bool = false;
	for i: 0..symbol_table.num_symbols-1 {
		if symbol_table.symbols[i].name == name {
			value   = symbol_table.symbols[i].value;
			success = true;
			break;
		}
	}

	return value, success;
}

evaluate :: ( node: *Ast_Node, env: *Environment, interp: *Interpreter ) {

	// Possible root nodes:
	// (1) Ident = Expression;
	// (2) Expression;

	reset_interpreter( interp );

	// assume it is an assignment statement; if not handle all other expressions
	binary_operator := cast(*Binary_Operator_Node)node;

	if node.kind == .Binary_Op && binary_operator.op == .Assign {
		assert( binary_operator.left.kind == .Identifier );

		ident 	   := cast(*Identifier_Node)binary_operator.left;
		expression := binary_operator.right;
		
		// eval and assign
		eval( expression, env, interp );
		multielement := build_multielement( interp, env );
		update_symbol( ident.name, multielement, *interp.symbol_table );
	}
	else {
		// evaluate every other type of expression
		eval( node, env, interp );
		multielement := build_multielement( interp, env );
		print( "Result: %\n", multielement_to_expression( multielement, env.basis_elements ) );
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

multielement_to_ast :: ( multielement: Multi_Element ) -> ( ast: *Ast_Node ) {

	// The 'new_...node' procedures allocate in temporary storage
	// Will get wiped every execution of the main loop

	ast: *Ast_Node = null;
	if multielement.count == 1 {
		ast = new_k_element_node( coefficient = multielement[0].coefficient, multielement[0].basis );
	}
	else {
		root := new_binary_operator_node( .Sum, null, null );

		i: int = 0;
		current := root;
		while i < multielement.count-2 {
			defer i += 1;
			current.left = new_k_element_node( coefficient = multielement[i].coefficient, multielement[i].basis );

			next := new_binary_operator_node( .Sum, null, null );
			current.right = next;
			current = next;
		}

		// the final sum needs handled outside the loop
		current.left  = new_k_element_node( coefficient = multielement[i].coefficient,   multielement[i].basis );
		current.right = new_k_element_node( coefficient = multielement[i+1].coefficient, multielement[i+1].basis );

		ast = root;
	}

	return ast;	
}

multielement_to_expression :: ( multielement: Multi_Element, basis_elements: [] Basis_Element ) -> ( expression: string ) {
	
	// DOES STRING BUILDER USE TEMPORARY STORAGE???

	// Grade 0 elements have an empty string as their 'symbol'
	// When printing a grade 0 the coefficient will only be reflected

	builder: String_Builder;
	basis:       s64;
	coefficient: string;

	i := 0;
	while i < multielement.count-1 {
		basis       = multielement[i].basis;
		coefficient = tprint( "%", multielement[i].coefficient );

		append( *builder, coefficient );
		append( *builder, basis_elements[basis].symbol );
		append( *builder, " + " );

		i += 1;
	}

	basis 		 = multielement[i].basis;
	coefficient  = tprint( "%", multielement[i].coefficient );
	append( *builder, coefficient );
	append( *builder, basis_elements[basis].symbol );

	expression: string = builder_to_string( *builder );

	return expression;
}

update_symbol :: ( name: string, value: Multi_Element, symbol_table: *Symbol_Table ) {

	// if the symbol table gets 'big' for whatever that means we should make this a hashtable
	i: int = 0;
	found: bool = false;
	while i < symbol_table.num_symbols {
		if symbol_table.symbols[i].name == name {
			found = true;
			break;
		}
		i += 1;
	}

	if found {
		// found an existing symbol, update it
		memcpy( symbol_table.symbols[i].value.data, value.data, value.count * size_of(K_Element) );
		symbol_table.symbols[i].value.count = value.count;
	}
	else {
		// did not find an existing symbol, add it provided we have enough space
		assert( i < Symbol_Table.MAX_NUM_SYMBOLS );
		assert( name.count <= Symbol_Table.SYMBOL_NAME_MAX_SIZE );

		memcpy( symbol_table.symbols[i].name.data,  name.data, name.count );
		symbol_table.symbols[i].name.count = name.count;

		memcpy( symbol_table.symbols[i].value.data, value.data, value.count * size_of (K_Element) );
		symbol_table.symbols[i].value.count = value.count;

		symbol_table.num_symbols += 1;
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

build_multielement :: ( interp: *Interpreter, env: *Environment ) -> ( multielement: Multi_Element ) {

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

	multielement: Multi_Element;

	while !stack_is_empty( *interp.stack ) {
		element, success := stack_pop( *interp.stack );
		display_index := env.basis_elements[element.basis].display_order;

		interp.multielement_builder[display_index].coefficient += element.coefficient;
		interp.multielement_builder[display_index].basis        = element.basis; // use this as an implicit flag to indicate this index was updated
	}

	// filter/pack the multielement to only include non-empty indices
	// [s1e1, ... s2e1^e2 ... s3e1^e2^e3] will be condensed down to [s1e2,s2e1^e2,s3e1^e2^e3]

	num_scalar_terms: int = 0;
	end: int = 0;
	for i: 0..env.num_basis_elements-1 {
		if interp.multielement_builder[i].basis >= 0 {
			if interp.multielement_builder[i].basis == 0 {
				num_scalar_terms += 1;
			}
			interp.multielement_builder[end] = interp.multielement_builder[i];
			interp.num_terms_in_multielement += 1;
			end += 1;	
		}
	}

	if num_scalar_terms > 0 {
		interp.num_terms_in_multielement -= (num_scalar_terms - 1); // count scalars a single time
	}

	multielement.data  = interp.multielement_builder.data;
	multielement.count = interp.num_terms_in_multielement; 

	return multielement;
}
