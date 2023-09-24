#scope_export

Line_Number 	 :: #type, distinct int;
Character_Index  :: #type, distinct int;

Range :: struct(T: Type) {
	start: T;
	end:   T;
}

Token_Type :: enum u8 {

	Undefined;
	Error;
	End_Of_Input;
	
	Identifier;
	Basis_Element;  // based on the currently declared set of basis symbols
	Literal; 		// quoted values like 'foo' or 'bar'
	Number;

	// Keywords are parsed into separate categories.
	// Those that are 'commands' and those that are 'configs'.
	// Set sentinel values for start and end for each category to ease parsing.
	
	Command_Start;	
	Keyword_Get;
	Keyword_Set;
	Keyword_Exit;
	Command_End;

	Config_Start;
	Keyword_Dimension;
	Keyword_Basis;
	Config_End;

	// separators
	Semicolon;
	Open_Paren;
	Close_Paren;
	Open_Square_Brace;
	Close_Square_Brace;
	Comma;

	// operators
	Plus;
	Minus;
	Assign; // single equals sign
	Exterior_Product; 
	Regressive_Product;
	Interior_Product;
	Left_Complement;
	Right_Complement;
}

Token :: struct {
	
	type: Token_Type = .Error;

	line_number: 	 Range(Line_Number);
	character_index: Range(Character_Index);

	union {
		identifier: string;
		text:    	string; // literal's 'value'
		number:     int;
		basis: 		s64; 	// for basis-elements
	}	
}
