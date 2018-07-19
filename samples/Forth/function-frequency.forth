' noop is bootmessage

\ --- LIST OF CONSTANTS
\ WORD#		maximum word size
\ RING#		size of `Rings' element
\ DEFS		definitions
\ KEYS
\
\ --- LIST OF VARIABLES
\ cmpl?		is compiling?
\ cword		current compiled word

    wordlist constant DEFS
    wordlist constant KEYS

\ --- Compiling
50 constant WORD#
: >>fPAD	( ca u -- ; u < 51 )
	PAD 80 blank s" create " PAD swap MOVE
	s"  1 , DOES> 1 swap +! ;" PAD 57 + swap MOVE
	WORD# min PAD 7 + swap MOVE ;

: funcmpl	( ca u -- )
	>>fPAD current @ DEFS current !
	PAD 80 evaluate current ! ;

: >>kPAD	( ca u -- ; )
	PAD 80 blank s" : " PAD swap MOVE
	s"  parse-name funcmpl ;" PAD 59 + swap MOVE
	WORD# min PAD 2 + swap MOVE ;

: keycmpl	( ca u -- )
	 >>kPAD current @ KEYS current !
	PAD 80 evaluate current ! ;

\ --- Interpreter
: intp	BEGIN parse-name dup
	WHILE	( ca u )
		2dup KEYS search-wordlist
		IF   execute 2drop
		ELSE DEFS search-wordlist IF execute THEN
		THEN
	REPEAT 2drop ;

: run 	BEGIN refill WHILE intp REPEAT ;

\ --- Lists&Rings
warnings OFF
: LIST	( node -- )	]] BEGIN @ dup WHILE >R [[ ; immediate
warnings ON
: LOOP-LIST	( -- )	]] R> REPEAT drop [[ ; immediate

: empty-ring?	( node -- f )	dup @ = ;
: RING	( node -- )	]] dup BEGIN @ 2dup <> WHILE 2>R [[ ; immediate
: LOOP-RING	( -- )	]] 2R> REPEAT 2drop [[ ; immediate

: new-node	( -- node )
	here dup , ;
: do-link		( node new-node  -- ; do link after current node )
	over @ over ! swap ! ;

\ --- Sorting..
: nt>freq	( nt -- n ;frequency of uses )
	name>int >BODY @ ;

: @maxfreq	( wid -- n ;maximum frequency )
	0 swap cell+
	LIST	( max )
		I nt>freq 2dup <
		IF nip ELSE drop THEN
	LOOP-LIST ;

    2 cells constant RING#
: rings-vec	( u -- a size ; create vector of rings )
	here over 1+ 0
	DO new-node drop 0 , LOOP
	swap RING# * ;

: populate-by 	( a wid -- )
	cell+
	LIST
		dup  I nt>freq RING# *   +	\ root-node
		new-node I ,			\ new-node
		do-link
	LOOP-LIST drop ;

\ --- Display TOP
: node>nt	cell+ @ ;

: .ring		( root-node -- )
	0 swap
	RING
		dup 0= IF I node>nt nt>freq . THEN
		space I node>nt name>string type
		1+
	LOOP-RING drop cr ;

: .top	( a size n -- )
	-rot BOUNDS swap
	?DO	( n )
		I empty-ring?	0= IF 1- I .ring THEN
		dup 		0= IF drop UNLOOP EXIT THEN
	[ RING# negate ] LITERAL +LOOP drop ;

: args>top#	( -- n )
	1 arg 2dup 0 0 d<>
	IF	>float
		IF 	f>d d>s dup 0= IF drop 4 THEN
		ELSE 	4 THEN
	ELSE	2drop 4 THEN ;


\ --- KEYS behaviour
    variable  cmpl?	cmpl? OFF
    2variable cword
here WORD# allot 0 cword 2!

current @ KEYS current !
: create
	cmpl? @
	IF   cword 2@   keycmpl
	ELSE parse-name funcmpl THEN ;

: constant
	cmpl? @
	IF   cword 2@   keycmpl
	ELSE parse-name funcmpl THEN ;

: variable	parse-name funcmpl ;
: value		parse-name funcmpl ;
: defer		parse-name funcmpl ;

: (	BEGIN >in @ [char] ) parse nip >in @ rot - =
	WHILE refill 0= IF exit THEN REPEAT ;
: \	10 parse 2drop ;
: \G	10 parse 2drop ;
: S"	[char] " parse 2drop ;
: ."	[char] " parse 2drop ;

: [']
	parse-name DEFS search-wordlist IF execute THEN ;
: postpone
	parse-name DEFS search-wordlist IF execute THEN ;

: ; 	cmpl? OFF ;
: :	warnings OFF
	parse-name
	cword 2@ drop WORD# rot  umin dup >R MOVE
	cword 2@ drop R> cword 2!
	cword 2@ cmpl? @
	IF	keycmpl		\ `:' inside def. = a defining word
	ELSE	funcmpl	THEN
	cmpl? ON
	warnings ON
;
current !

\ Run, ruuun!
stdin ' run execute-parsing-file  DEFS @maxfreq rings-vec  over DEFS populate-by  args>top# .top bye
