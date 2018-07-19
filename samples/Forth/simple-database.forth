\ sdb.fs	Simple database. Gforth 0.7.0 specific
' noop is bootmessage

wordlist constant USER:  \ USER functions
wordlist constant SDB    \ DB keys
wordlist constant FIELDS \ DB fields

: -SDB?EXIT	( -- ; Continue if `SDB' non-empty )
	SDB cell+ @ 0= IF rdrop exit THEN ;

\ MACROS
: |comp|	( MACRO: | code to be compiled | )
	char parse evaluate ; immediate
: LIST	( node -- )	]] BEGIN @ dup WHILE >R [[ ; immediate
: LOOP-LIST	( -- )	]] R> REPEAT drop [[ ; immediate
: UNLIST	( -- )	POSTPONE rdrop ; immediate

\ Helper words
: .;	[char] ; emit ;
: !+	dup cell+ -rot ! ;
: ($+)	( cas us cad ud -- cad us+ud ; append source string to destination )
	swap >R	2dup + R> swap >R tuck + swap >R swap move R> R> ;

\ --- Working Record Fields
current @ FIELDS current !
2variable person
2variable birthday
2variable relationship
2variable hobby
current ! FIELDS >order

\ --- Ring list
: new-node	( -- node ; root of circular list )
	here dup , ;
: do-link	( node new-node  -- ; do link after current node )
	over @ over ! swap ! ;
: empty-ring?	( node -- f )	dup @ = ;

: RING	( node -- )	]] dup BEGIN @ 2dup <> WHILE 2>R [[ ; immediate
: LOOP-RING	( -- )	]] 2R> REPEAT 2drop [[ ; immediate
: UNRING	( -- )	postpone 2RDROP ; immediate

:noname
	s" " person 2!  s" 9999.01.01" birthday 2!  s" " relationship 2!  s" " hobby 2! ;
new-node swap , CONSTANT OLD

\ --- Insertion into ring
: node>xt	( node -- xt )		cell+ @ ;
: bday@		( node -- ca u )	node>xt execute birthday 2@ ;
: datecmp	( node node -- -1|0|1 )	bday@ rot bday@ compare ;

: search-by-date	( new-node node -- node' ; Linear search by birth date )
	dup
	RING	( new-node node )
		over I datecmp 0<
		IF   nip  UNRING exit
		ELSE drop I THEN
	LOOP-RING    nip ;

: insert-ordered	( new-node -- )
	OLD dup empty-ring? 0=
	IF over swap search-by-date THEN swap do-link ;

\ --- Field compiling
: fld:   [char] ; parse postpone sliteral ; immediate

: _sdb-walk	( MACRO: | xt[--] -- | )
	]] -SDB?EXIT here dup SDB cell+
	LIST I name>int swap !+ LOOP-LIST [[  1 cells ]] literal -
	DO   I @ |comp| [[ -1 cells ]] literal +LOOP [[ ; immediate

2variable category
: msg_nocategory	( -- )
	." No such category!" cr ;

\ --- USER definitions
user: >order current @ USER: current !
: bye bye ;
: .s .s ;
: newline cr ;
: godb SDB >order ;
: .keys  SDB wordlist-words ;
: .person	person  2@ type ;
: .birthday	birthday  2@ type ;
: .relationship	relationship 2@ type ;
: .hobby	hobby 2@ type ;
: .record	.person 1 spaces .birthday .; .relationship .; .hobby .; ;

: .last  SDB cell+ @ dup
	IF name>int execute .record ELSE drop THEN ;

: .lastbycategory	( "field-name" -- )
	here 0 category 2! parse-name FIELDS search-wordlist
	IF	sdb cell+
		LIST	( xt )
			I name>int execute dup execute 2@ category 2@ 2swap search nip nip 0=
			IF .record cr dup execute 2@ category 2@ ($+) category 2! THEN
		LOOP-LIST drop
	ELSE    ['] msg_nocategory stderr OUTFILE-EXECUTE THEN ;

: .bydate	( -- )
	OLD RING I node>xt execute .record cr LOOP-RING ;

: .sdb		_sdb-walk | execute .record cr | ;
: sdb-dump	_sdb-walk | execute ." +: " .record cr | ;

: dump	( "db-file" -- )
	parse-name w/o
	CREATE-FILE throw dup
		['] sdb-dump swap OUTFILE-EXECUTE
	CLOSE-FILE throw ;

: load	( "db-file" -- )	include ;

: +:	( "person birth-day;relationship;hobby; -- )
	new-node dup cell+ >R 0 ,			\ post    `xt'
	current @ SDB current !				\ compile into `sdb' wordlist
	: latestxt dup R> !				\ fix-up  `xt'
	  ]] literal >name name>string person 2! fld: birthday 2!  fld: relationship 2!  fld: hobby 2! ; [[
	current !
	insert-ordered ;

: HELP	cr
	cr ." KEYWORDS: " USER: wordlist-words cr ." ---"
	cr ." Prefix `.' means print"
	cr ." To add a record, type: +: name yyyy.mm.dd;relationship;hobby;"
	cr cr ;
current !
SEAL godb
