\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( Tools words. )

: .s ( -- )
    [char] < emit  depth (.)  ." > "
    'SP @ >r r@ depth 1- cells +
    begin
	dup r@ <>
    while
	dup @ .
	/cell -
    repeat r> 2drop ;

: ?   @ . ;

: c?   c@ . ;

: dump   bounds do i ? /cell +loop cr ;

: cdump   bounds do i c? loop cr ;

: again   postpone branch , ; immediate

: see-find ( caddr -- end xt )
    >r here lastxt @
    begin
	dup 0= abort" Undefined word"
	dup r@ word= if r> drop exit then
	nip dup >nextxt
    again ;

: cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: xt. ( xt -- )
    ( >name ) count cabs type ;

: xt? ( xt -- flag )
    >r lastxt @ begin
	?dup
    while
	dup r@ = if r> 2drop -1 exit then
	>nextxt
    repeat r> drop 0 ;

: disassemble ( x -- )
    dup xt? if
        ( >name ) count
        dup 127 > if ." postpone " then
        cabs type
    else
        .
    then ;

: .addr  dup . ;

: see-line ( addr -- )
    cr ."     ( " .addr ." ) "  @ disassemble ;

: see-word ( end xt -- )
    >r ." : " r@ xt.
    r@ >body do i see-line /cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see   bl word see-find see-word cr ;

: #body   bl word see-find >body - ;

: type-word ( end xt -- flag )
    xt. space drop 0 ;

: traverse-dictionary ( in.. xt -- out.. )
    \ xt execution: ( in.. end xt2 -- in.. 0 | in.. end xt2 -- out.. true )
    >r  here lastxt @  begin
	?dup
    while
	r> 2dup >r >r execute
	if r> r> 2drop exit then
	r> dup >nextxt
    repeat r> 2drop ;

: words ( -- )
    ['] type-word traverse-dictionary cr ;

\ ----------------------------------------------------------------------

( Tools extension words. )

\ ;code

\ assembler

\ in kernel: bye

\ code

\ cs-pick

\ cs-roll

\ editor

: forget   ' dup >nextxt lastxt !  'here !  reveal ;

\ Kernel: state

\ [else]

\ [if]

\ [then]

\ ----------------------------------------------------------------------

( Forth2012 tools extension words. )

\ TODO: n>r

\ TODO: nr>

\ TODO: synonym

: [undefined]   bl-word find nip 0= ; immediate

: [defined]   postpone [undefined] invert ; immediate

\ ----------------------------------------------------------------------

: @+ ( addr -- addr+/cell x )   dup cell+ swap @ ;

: !+ ( x addr -- addr+/cell )   tuck ! cell+ ;

: -rot   swap >r swap r> ;
