//  Parse JSON
//
//  Nigel Galloway - April 27th., 2012
//
grammar JSON ;
@members {
String Indent = "";
}
Number	:	(('0')|('-'? ('1'..'9') ('0'..'9')*)) ('.' ('0'..'9')+)? (('e'|'E') ('+'|'-')? ('0'..'9')+)?;
WS	:	(' ' | '\t' | '\r' |'\n') {skip();};
Tz	:	' ' .. '!' | '#' .. '[' | ']' .. '~';
Control	:	'\\' ('"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t'|UCode);
UCode	:	'u' ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F');
Keyword	:	'true' | 'false' | 'null';
String	:	'"' (Control? Tz)* '"';
object	:       '{' {System.out.println(Indent + "{Object}"); Indent += "    ";} (pair (',' pair*)*)? '}' {Indent = Indent.substring(4);};
pair	:	e = String {System.out.println(Indent + "{Property}\t" + $e.text);} ':' value;
value	:	Number             {System.out.println(Indent + "{Number}  \t" + $Number.text);}
	|	object
	|	String             {System.out.println(Indent + "{String}  \t" + $String.text);}
	|	Keyword            {System.out.println(Indent + "{Keyword} \t" + $Keyword.text);}
	|	array;
array	:	'[' {System.out.println(Indent + "Array"); Indent += "    ";} (value (',' value)*)? ']' {Indent = Indent.substring(4);};
