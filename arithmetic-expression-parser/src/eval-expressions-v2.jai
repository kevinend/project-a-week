/*
	Parses and evaluates simple mathematical expressions which handles operator precedence
	without the use of parenthesis. The Backus-Naur forms (BNF) the parsing routines
	handle is below. See main for examples that fit the pattern vs those that do not.

	BNF for an expression, term, and factor.
	expression ::= [-]<term> [("+","-") <term>]...
	term       ::= <factor>  [("*","/") <factor>]...
	factor     ::= <number> | "(" <expression> ")"
*/

#scope_export

parse_expression :: ( _expression: string ) -> ( value: float, expression: string ) {
	term: float;	
	expression := skip_spaces( _expression );

	term_is_negative: bool = false;
	if expression[0] == #char "-" {
		term_is_negative = true;
		advance( *expression, 1 );
	}

	term, expression = parse_term( expression );
	if term_is_negative { term *= -1; }
	
	expression = skip_spaces( expression );

	while expression && (expression[0] == #char "+" || expression[0] == #char "-") {
		op: u8;
		next_term: float;

		op = expression[0];
		advance( *expression, 1 );
		expression = skip_spaces( expression );
		next_term, expression = parse_term( expression );

		if op == #char "+" { term += next_term; }
		else 
		if op == #char "-" { term -= next_term; }
	}

	return term, expression;
}

main :: () {

// Testing the parsing routines, can handle operator precedence with or without parens.
// Parsing routines are really parsers and eval procedures.
// Next stop is to store these in an expression tree then evaluate on-demand.

{
	expression: string = "3";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "-3";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "3 + 4";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "-3 + 4";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "3 * 4";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "3*4+5";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "3+4*5";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "(3+4)*5+3+2";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "(3+4)*5+3+-2";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

{
	expression: string = "((3+4)/7 + (3-2))";
	value: float = parse_expression( expression );
	print( "Value is %\n", value );
};

	return;
}

#scope_file

#import "Basic";

skip_spaces :: ( _s: string ) -> string {
	s := _s;
	while s && (s[0] == #char " " || s[0] == 9) { // ASCII for horizontal tab
		advance( *s, 1 );
	}
	return s;
}

parse_term :: ( _expression: string ) -> ( factor: float, expression: string ) {
	
	expression := skip_spaces( _expression );

	factor: float;
	factor, expression = parse_factor( expression );

	expression = skip_spaces( expression );

	while expression && (expression[0] == #char "*" || expression[0] == #char "/") { 
		op: u8;
		next_factor: float;

		op = expression[0];
		advance( *expression, 1 );
		expression = skip_spaces( expression );
		next_factor, expression = parse_factor( expression );

		if op == #char "*" { factor *= next_factor; }
		else 
		if op == #char "/" { factor /= next_factor; }
	}

	return factor, expression;
}

parse_factor :: ( _expression: string ) -> (value: float, expression: string) {

	expression := skip_spaces( _expression );

	value: float;

	if is_digit( expression[0] ) {
		success: bool = false;
		value, success, expression = string_to_float( expression );
	}
	else if expression[0] == #char "(" {
		advance( *expression, 1 );
		value, expression = parse_expression( expression );
		expression = skip_spaces( expression );
		if expression[0] == #char ")" {
			advance( *expression, 1 );
		}
		else {
			print( "Error, expected a closing paren to terminate the expression" );
			exit( -1 );
		}
	}

	return value, expression;
}


