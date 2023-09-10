// READ   -- return a specific element at an index
// SEARCH -- search through the data structure looking for a specific element
// INSERT -- adding an element to the data structure while maintaining the invariants
// DELETE -- remove an element from the data structure, while again maintaining the invariants

#scope_export

Stack :: struct ($T: Type) {
	head: int;
	num_entries: int;
	entries: [] T; // '.count' is the maximum number of entries allowed

	allocator: Allocator;
}

stack_init :: ( max_num_elements: int, $E: Type, allocator: Allocator = context.allocator ) -> Stack(E) {

	Basic.assert( max_num_elements > 0 );

	stack: Stack(E);
	stack.entries.data = Basic.alloc( size = (max_num_elements * size_of(E)), allocator = allocator );
	if !stack.entries.data {
		Basic.print( "Unable to allocate space for the stack elements" );
	}
	stack.entries.count = max_num_elements;

	stack.allocator = allocator;

	return stack;
}

stack_deinit :: ( stack: *Stack ) {

	Basic.assert( stack != null );
	Basic.assert( stack.entries.data != null );

	Basic.free( stack.entries.data );
	stack.entries.data = null;
	stack.num_entries  = 0;
	
	return;
}

stack_push :: insert;
stack_pop  :: remove_;
stack_peek :: stack_head;

stack_head :: inline (stack: Stack($T)) -> int {
	return read( stack, stack.head - 1 );	
}

stack_is_full :: inline (stack: Stack($T)) -> bool #must {
	return (stack.num_entries == stack.entries.count);
}

stack_is_empty :: inline (stack: Stack($T)) -> bool #must {
	return (stack.num_entries == 0);
}

#scope_file
Basic :: #import "Basic";

read :: inline (stack: Stack($T), index: int) -> (int) {
	return stack.entries[index];
}

search :: (stack: Stack($T), value: T) -> ( index: int, found: bool #must ) {
	
	// The item type T must support == for equality checks!
	// Need to search from head to tail, old entries might still be in the stack and haven't been overwritten yet!

	found := false;
	index := -1;
	next  := stack.head;
	for i: 0..stack.num_entries-1 { 
		if read(stack,next) == value {
			found = true;
			index = next;
			break;
		}

		next += 1;
	}

	return found, index;
}

insert :: (stack: *Stack($T), value: T) -> (success: bool #must) {

	if stack_is_full( <<stack ) {
		Basic.print( "Stack insertion failed. The stack is full!\n" );
		return false;
	}

	using stack;
	entries[head] = value;
	head += 1;
	num_entries += 1;

	return true;
}

remove_ :: (stack: *Stack($T)) -> (value: T, removed: bool #must ) {

	
	if stack_is_empty( <<stack ) {
		Basic.print( "Stack removal failed. The stack is empty!\n" );
		dummy: T; // gets whatever default value or an initiliazed value of type T
		return dummy, false;
	}

	// assumes the user has code running in a loop
	// and writes something like while !is_empty( stack ) { remove stuff }

	using stack;
	removed := entries[head-1];
	head -= 1;
	num_entries -= 1;

	return removed, true;
}
