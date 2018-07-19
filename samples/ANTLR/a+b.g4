grammar aplusb ;

options {
	language = Java;
}

aplusb	:	(WS* e1=Num WS+ e2=Num NEWLINE {System.out.println($e1.text + " + " + $e2.text + " = " + (Integer.parseInt($e1.text) + Integer.parseInt($e2.text)));})+
	;
Num	:	'-'?('0'..'9')+
	;
WS	:	(' ' | '\t')
	;
NEWLINE	:	WS* '\r'? '\n'
        ;
