#scope_export

MAX_USER_INPUT_LENGTH 		:: 100;
CARRIAGE_RETURN_AND_NEWLINE :: 2;
USER_INPUT_BUFFER_SIZE 		:: MAX_USER_INPUT_LENGTH + CARRIAGE_RETURN_AND_NEWLINE;

user_input_buffer: [USER_INPUT_BUFFER_SIZE] u8;
std_in: File;

read_line :: () -> ( user_input: string ) {

	if !std_in.handle {
		std_in.handle = GetStdHandle( STD_INPUT_HANDLE );
	}

	// Read user input using terminal/console.
	user_input: string;
	user_input.data = user_input_buffer.data;

	while true {
		bytes_read: u32;
		success := ReadFile( std_in.handle, user_input_buffer.data, cast(u32)USER_INPUT_BUFFER_SIZE, *bytes_read, null );
		if bytes_read > 0 {
			user_input.count = bytes_read;
			if user_input.data[user_input.count-1] == #char "\n" { // read a whole line from standard in
				break;
			}
			else {
				// Input exceeds the fixed buffer length.
				// Ignore the input. Flush stdin. Continue to the next iteration.
				print( "Expression length exceeded limit of % characters, ignoring input\n", MAX_USER_INPUT_LENGTH );
				user_input.count = 0;

				// there has to be a better way to flush stdin...
				temp: u8;
				while true {
					success = ReadFile( std_in.handle, *temp, 1, *bytes_read, null );
					if temp == #char "\n" { break; }
				}

				continue;
			}
		}
	}

	return user_input;
}

// when you read a file you are not sure the number of bytes you will get back
// or how many compose a line
// so you want to try and read more or less, or you have to read a single file
// you want to read an entire file and then iterate through it and when you find the newline just deal with it then
