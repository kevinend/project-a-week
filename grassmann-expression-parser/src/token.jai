#scope_export

Line_Number 	 :: #type, distinct int;
Character_Index  :: #type, distinct int;

Range :: struct(T: Type) {
	start: T;
	end:   T;
}

Token_Type :: enum u8 {

	Error;
	End_Of_Input;
	
	Identifier;

	Keyword_Dimension;
	Keyword_Basis_Elements;
	Keyword_Help;

	Basis_Element;

	Number;

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
		identifier:  		 string;
		number:      		 int;
		basis_element_index: int;
	}	
}
