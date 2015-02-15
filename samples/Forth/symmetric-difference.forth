: elm	( n -- ; one cell per set )
	[ cell 8 * 1- ] literal umin CREATE 1 swap lshift ,
DOES> 	( -- 2^n ) @ ;

: universe	( u "name" -- )
	dup 0 DO I elm latest swap LOOP
	CREATE dup , 0 DO , LOOP
	DOES>  ( n a -- )  dup @ tuck cells +
		swap 0
		DO	( n a' )
			over I rshift 1 AND
			IF dup @ name>string space type THEN
			1 cells -
		LOOP	2drop ;

5 universe john bob mary serena jim	persons
john bob mary serena or or or
jim mary john bob    or or or

2dup xor           persons
2dup -1 xor and cr persons
swap -1 xor and cr persons
cr bye
