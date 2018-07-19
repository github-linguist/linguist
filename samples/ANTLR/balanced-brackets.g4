grammar balancedBrackets ;

options {
	language = Java;
}

bb	:	{System.out.print("input is: ");} (balancedBrackets {System.out.print($balancedBrackets.text);})* NEWLINE {System.out.println();}
	;
balancedBrackets
	:	OpenBracket balancedBrackets* CloseBracket
	;
OpenBracket
	:	'['
	;
CloseBracket
	:	']'
	;
NEWLINE	:	'\r'? '\n'
	;
