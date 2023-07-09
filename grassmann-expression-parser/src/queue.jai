// READ   -- return a specific element at an index
// SEARCH -- search through the data structure looking for a specific element
// INSERT -- adding an element to the data structure while maintaining the invariants
// DELETE -- remove an element from the data structure, while again maintaining the invariants

#scope_export

Queue :: struct($T: Type) {

	head:  int;
	tail:  int;				

	num_entries: int;
	entries: [] T; 			// '.count' is the maximum number of entries allowed 

	allocator: Allocator;
}

queue_init :: ( max_num_elements: int, $E: Type, allocator: Allocator = context.allocator ) -> Queue(E) {

	Basic.assert( max_num_elements > 0 );

	queue: Queue(E);
	queue.entries.data = Basic.alloc( size = (max_num_elements * size_of(E)), allocator = allocator );
	if !queue.entries.data {
		Basic.print( "Unable to allocate space for the queue elements" );
	}
	queue.entries.count = max_num_elements;

	queue.allocator = allocator;

	return queue;
}

queue_deinit :: (queue: *Queue ) {

	Basic.assert( queue != null );
	Basic.assert( queue.entries.data != null );

	Basic.free( queue.entries.data );
	queue.entries.data = null;
	queue.num_entries  = 0;
	
	return;
}

enqueue :: insert;
dequeue :: remove_;
peek    :: head;

head :: inline (queue: Queue($T)) -> int {
	return read( queue, queue.head );	
}

tail :: inline (queue: Queue($T)) -> int {
	// .tail's index will point to the next available entry, so the 'tail' is really the prior inserted element
	return read( queue, queue.tail - 1 );
}

is_full :: inline (queue: Queue($T)) -> bool #must {
	return (queue.num_entries == queue.entries.count);
}

is_empty :: inline (queue: Queue($T)) -> bool #must {
	return (queue.num_entries == 0);
}

// @TODO: Lookup how Jon runs test and maybe we can make our first test suite!
//        That runs at compile-time so we can check whether the build would fail or not
//        Or we build and at the end of the build we run our tests, however it needs to work
tests :: () {

	Basic.print( "size_of(Queue) == %\n", size_of(Queue(int)) );

	queue: Queue(int) = init( 5, int );
	Basic.print( "Queue.T == %\n", queue.T );

	{
		found,index := search( queue, 10 );
		if !found { Basic.print( "Expected!\n"); }
		Basic.print( "Number of queue entries is %\n", queue.num_entries );
	}

	success := insert( *queue, 10 );
	success =  insert( *queue, 20 );
	success =  insert( *queue, 30 );
	success =  insert( *queue, 40 );
	success =  insert( *queue, 50 );
	success =  insert( *queue, 60 );
	success =  insert( *queue, 70 );
	success =  insert( *queue, 60 );
	success =  insert( *queue, 70 );

	p := peek( queue );
	h := head( queue );
	t := tail( queue );

	found, index := search( queue, 10 );
	if found {
		Basic.print( "Index is %\n", index );
	}

	found, index  = search( queue, 20 );
	if found {
		Basic.print( "Index is %\n", index );
	}
		
	found, index  = search( queue, 50 );
	if found {
		Basic.print( "Index is %\n", index );
	}

	found, index = search( queue, 100 );
	if !found {
		Basic.print( "Didn't find the element, index returned by the function is %\n", index );
	}

	Basic.print( "(p,h,t) == (%,%,%)\n", p, h, t );

	while !is_empty( queue ) {
		removed, value := dequeue( *queue );
		Basic.print( "value == %\n", value );

		p = peek( queue );
		h = head( queue );
		t = tail( queue );

		Basic.print( "(p,h,t) == (%,%,%)\n", p, h, t );
	}

	success = insert( *queue, 70 );
	p = peek( queue );
	h = head( queue );
	t = tail( queue );

	Basic.print( "(p,h,t) == (%,%,%)\n", p, h, t );
	Basic.print( "queue == %\n", queue );

	success = insert( *queue, 80 );
	p = peek( queue );
	h = head( queue );
	t = tail( queue );

	Basic.print( "(p,h,t) == (%,%,%)\n", p, h, t );
	Basic.print( "queue == %\n", queue );

	// i don't know man that looks okay to me

	return;
}

#scope_file

Basic :: #import "Basic";

read :: inline (queue: Queue($T), index: int) -> (int) {
	return queue.entries[index];
}

search :: (queue: Queue($T), value: T) -> (found: bool #must, index: int) {
	
	// The item type T must support == for equality checks!
	// Need to search from head to tail, old entries might still be in the queue and haven't been overwritten yet!

	found := false;
	index := -1;
	next  := queue.head;
	for i: 0..queue.num_entries-1 { 
		if read(queue,next) == value {
			found = true;
			index = next;
			break;
		}

		if next == queue.entries.count {
			// wrap around and start over
			next = 0;
		}
		else {
			next += 1;
		}
	}

	return found, index;
}

insert :: (queue: *Queue($T), value: T) -> (success: bool #must) {

	if is_full( <<queue ) {
		Basic.print( "Queue insertion failed. The queue is full!\n" );
		return false;
	}

	using queue;
	if tail == entries.count {
		tail = 0;
	}
	entries[tail] = value;
	tail += 1;
	num_entries += 1;

	return true;
}

remove_ :: (queue: *Queue($T)) -> ( value: T, success: bool #must ) {

	// assumes the user has code running in a loop
	// and writes something like while !is_empty( queue ) { remove stuff }

	using queue;
	removed := entries[head];
	head += 1;
	if head == entries.count {
		head = 0;
	}
	num_entries -= 1;

	return removed, true;
}
