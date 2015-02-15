/* Parse Roman Numerals

   Nigel Galloway March 16th., 2012
*/
grammar ParseRN ;

options {
	language = Java;
}
@members {
int rnValue;
int ONE;
}

parseRN:	({rnValue = 0;} rn NEWLINE {System.out.println($rn.text + " = " + rnValue);})*
	;
	
rn	:	(Thousand {rnValue += 1000;})* hundreds? tens? units?;

hundreds:	{ONE = 0;} (h9 | h5) {if (ONE > 3) System.out.println ("Too many hundreds");};
h9	:	Hundred {ONE += 1;} (FiveHund {rnValue += 400;}| Thousand {rnValue += 900;}|{rnValue += 100;} (Hundred {rnValue += 100; ONE += 1;})*);
h5	:	FiveHund {rnValue += 500;} (Hundred {rnValue += 100; ONE += 1;})*;

tens	:	{ONE = 0;} (t9 | t5) {if (ONE > 3) System.out.println ("Too many tens");};
t9	:	Ten {ONE += 1;} (Fifty {rnValue += 40;}| Hundred {rnValue += 90;}|{rnValue += 10;} (Ten {rnValue += 10; ONE += 1;})*);
t5	:	Fifty {rnValue += 50;} (Ten {rnValue += 10; ONE += 1;})*;
	
units	:	{ONE = 0;} (u9 | u5) {if (ONE > 3) System.out.println ("Too many ones");};
u9	:	One {ONE += 1;} (Five {rnValue += 4;}| Ten {rnValue += 9;}|{rnValue += 1;} (One {rnValue += 1; ONE += 1;})*);
u5	:	Five {rnValue += 5;} (One {rnValue += 1; ONE += 1;})*;
	
One	:	'I';
Five	:	'V';
Ten	:	'X';
Fifty	:	'L';
Hundred:	'C';
FiveHund:	'D';
Thousand:	'M' ;
NEWLINE:	'\r'? '\n' ;
