grammar rpnC ;
//
//  rpn Calculator
//
//  Nigel Galloway - April 7th., 2012
//
@members {
Stack<Double> s = new Stack<Double>();
}
rpn	:	(WS* (num|op) (WS | WS* NEWLINE {System.out.println(s.pop());}))*;
num	:	'-'? Digit+ ('.' Digit+)? {s.push(Double.parseDouble($num.text));};
Digit	:	'0'..'9';
op	:	'-' {double x = s.pop(); s.push(s.pop() - x);}
	|	'/' {double x = s.pop(); s.push(s.pop() / x);}
	|	'*' {s.push(s.pop() * s.pop());}
	|	'^' {double x = s.pop(); s.push(Math.pow(s.pop(), x));}
	|	'+' {s.push(s.pop() + s.pop());};
WS	:	(' ' | '\t'){skip()};
NEWLINE	:	'\r'? '\n';
