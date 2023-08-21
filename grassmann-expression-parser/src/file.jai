#scope_export

MAX_USER_INPUT_LENGTH 		:: 50;
CARRIAGE_RETURN_AND_NEWLINE :: 2;
USER_INPUT_BUFFER_SIZE 		:: MAX_USER_INPUT_LENGTH + CARRIAGE_RETURN_AND_NEWLINE;

user_input_buffer: [USER_INPUT_BUFFER_SIZE] u8;
std_in: File;

read_line :: () -> ( user_input: string ) {

	if !std_in.handle {
		std_in.handle = GetStdHandle( STD_INPUT_HANDLE );
	}

	// Read the input from the user through the terminal
	user_input: string;
	user_input.data = user_input_buffer.data;	
	
	while true {
		bytes_read: u32;
		success := ReadFile( std_in.handle, user_input_buffer.data, cast(u32)USER_INPUT_BUFFER_SIZE, *bytes_read, null );
		if bytes_read {
			user_input.count = bytes_read;
			if user_input.data[user_input.count-1] == #char "\n" { // read a whole line
				user_input.count -= CARRIAGE_RETURN_AND_NEWLINE;
				break; 
			}
			else {
				// Input exceeds the fixed buffer length
				// Ignore the input, flush stdin, and continue to the next iteration
				print( "Expression length exceeded limit of % characters ignoring input\n", MAX_USER_INPUT_LENGTH );
					
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
		else {
			// error, unable to read the requested number of bytes of input from standard in!
		}
	}

	return user_input;
}


#scope_file

#import "Windows"; // ReadFile()
