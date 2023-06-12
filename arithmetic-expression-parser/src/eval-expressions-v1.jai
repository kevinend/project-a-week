/*
	Parses and evaluates simple mathematical expressions that are wrapped in parens to handle
	operator precedence. The Backus-Naur form the parsing routines handle is below.
	See main for a sample of examples that fit the pattern as well as those that do not.

	BNF for an expression
	expression ::= <number> | "(" <expression> <operator> <expression> ")"
	operator   ::= "+" | "-" | "*" | "/"

	Handles expressions of the form:
	3
	(3 + 4)
	((3+4) * 1)
*/

#scope_export

skip_spaces :: ( _s: string ) -> string {
	s := _s;
	while s && (s[0] == #char " " || s[0] == 9) { // 9 is ASCII horizontal tab
		advance( *s, 1 );
	}
	return s;
}

parse_operator :: ( _expression: string ) -> ( op: u8, remainder: string )  {
	expression := skip_spaces( _expression );
	c := expression[0];
	if c == #char "+" || c == #char "-" || c == #char "*" || c == #char "/" {
		op: u8 = c;
		advance( *expression, 1 );
		return op, expression;
	}
	else {
		print( "Error, expected an operator for the expression" );
		exit(-1);
	}
}

parse_expression :: ( _expression: string ) -> ( value: float, remainder: string ) {
	value: float;
	expression := _expression;  // _expression is immutable by default
		
	expression = skip_spaces( _expression );

	if is_digit( expression[0] ) {
		success: bool = false;
		value, success, expression = string_to_float( expression ); 
		return value, expression;
	}
	else if expression[0] == #char "(" {
		advance( *expression, 1 );
		expression = skip_spaces( expression );

		op: u8;
		next_value: float;
	
		value, 	    expression = parse_expression( expression ); 
		op, 		expression = parse_operator( expression );
		next_value, expression = parse_expression( expression );

		expression = skip_spaces( expression );
		if expression[0] == #char ")" {
			advance( *expression, 1 );
			expression = skip_spaces( expression );

			if op == #char "+" {
				value += next_value;
			}
			else if op == #char "-" {
				value -= next_value;
			}
			else if op == #char "*" {
				value *= next_value;
			}
			else if op == #char "/" {
				value /= next_value;
			}

			return value, expression;
		}
		else {
			print( "Error, expected closing paren to terminate expression" );
			exit( -1 );
		}
	}
	else {
		print( "Error, unable to parse expression\n" );
		exit( -1 );
	}
}


main :: () {

// Testing the parsing routines, can handle expressions that are correctly wrapped in parens.
// Does not handle operator precedence without the use of parens.
// Even simple expressions like "3+4" will not parse correctly!

{
	expression: string = "3";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "(3*4)";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};


{
	expression: string = "((3*4)+5)";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

}

#scope_file

#import "Basic";
