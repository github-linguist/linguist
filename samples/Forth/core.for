: immediate   lastxt @ dup c@ negate swap c! ;

: \   source nip >in ! ; immediate \ Copyright 2004, 2012 Lars Brinkhoff

: char \ ( "word" -- char )
    bl-word here 1+ c@ ;

: ahead  here 0 , ;

: resolve   here swap ! ;

: '   bl-word here find 0branch [ ahead ] exit [ resolve ] 0 ;

: postpone-nonimmediate   [ ' literal , ' compile, ] literal , ;

: create   dovariable_code header, reveal ;

create postponers
    ' postpone-nonimmediate ,
    ' abort ,
    ' , ,

: word \ ( char "<chars>string<char>" -- caddr )
    drop bl-word here ;

: postpone \ ( C: "word" -- )
    bl word find 1+ cells  postponers + @ execute ; immediate

: unresolved \ ( C: "word" -- orig )
    postpone postpone  postpone ahead ; immediate

: chars \ ( n1 -- n2 )
    ;

: else \ ( -- ) ( C: orig1 -- orig2 )
    unresolved branch swap resolve ; immediate

: if \ ( flag -- ) ( C: -- orig )
    unresolved 0branch ; immediate

: then \ ( -- ) ( C: orig -- )
    resolve ; immediate

: [char] \ ( "word" -- )
    char  postpone literal ; immediate

: (does>)   lastxt @ dodoes_code over >code ! r> swap >does ! ;

: does>   postpone (does>) ; immediate

: begin \ ( -- ) ( C: -- dest )
    here ; immediate

: while \ ( x -- ) ( C: dest -- orig dest )
    unresolved 0branch swap ; immediate

: repeat \ ( -- ) ( C: orig dest -- )
    postpone branch ,  resolve ; immediate

: until \ ( x -- ) ( C: dest -- )
    postpone 0branch , ; immediate

: recurse   lastxt @ compile, ; immediate

: pad \ ( -- addr )
    here 1024 + ;

: parse \ ( char "string<char>" -- addr n )
    pad >r  begin
	source? if <source 2dup <> else 0 0 then
    while
	r@ c!  r> 1+ >r
    repeat  2drop  pad r> over - ;

: ( \ ( "string<paren>" -- )
    [ char ) ] literal parse 2drop ; immediate
    \ TODO: If necessary, refill and keep parsing.

: string, ( addr n -- )
    here over allot align  swap cmove ;

: (s") ( -- addr n ) ( R: ret1 -- ret2 )
    r> dup @ swap cell+ 2dup + aligned >r swap ;

create squote   128 allot

: s" ( "string<quote>" -- addr n )
    state @ if
	postpone (s")  [char] " parse  dup ,  string,
    else
	[char] " parse  >r squote r@ cmove  squote r>
    then ; immediate

: (abort") ( ... addr n -- ) ( R: ... -- )
    cr type cr abort ;

: abort" ( ... x "string<quote>" -- ) ( R: ... -- )
    postpone if  postpone s"  postpone (abort")  postpone then ; immediate

\ ----------------------------------------------------------------------

( Core words. )

\ TODO: #
\ TODO: #>
\ TODO: #s

: and  ( x y -- x&y )   nand invert ;

: *   1 2>r 0 swap begin r@ while
         r> r> swap 2dup dup + 2>r and if swap over + swap then dup +
      repeat r> r> 2drop drop ;

\ TODO: */mod

: +loop ( -- ) ( C: nest-sys -- )
    postpone (+loop)  postpone 0branch  ,  postpone unloop ; immediate

: space   bl emit ;

: ?.-  dup 0 < if [char] - emit negate then ;

: digit   [char] 0 + emit ;

: (.)   base @ /mod  ?dup if recurse then  digit ;

: ." ( "string<quote>" -- )   postpone s"  postpone type ; immediate

: . ( x -- )   ?.- (.) space ;

: postpone-number ( caddr -- )
    0 0 rot count >number dup 0= if
	2drop nip
	postpone (literal)  postpone (literal)  postpone ,
	postpone literal  postpone ,
    else
	." Undefined: " type cr abort
    then ;

' postpone-number  postponers cell+  !

: / ( x y -- x/y )   /mod nip ;

: 0< ( n -- flag )   0 < ;

: 1- ( n -- n-1 )   -1 + ;

: 2! ( x1 x2 addr -- )   swap over ! cell+ ! ;

: 2* ( n -- 2n )   dup + ;

\ Kernel: 2/

: 2@ ( addr -- x1 x2 )   dup cell+ @ swap @ ;

\ Kernel: 2drop
\ Kernel: 2dup

\ TODO: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
\           3 pick 3 pick ;

\ TODO: 2swap

\ TODO: <#

: abs ( n -- |n| )
    dup 0< if negate then ;

\ TODO: accept

: c, ( n -- )
    here c!  1 chars allot ;

: char+ ( n1 -- n2 )
    1+ ;

: constant   create , does> @ ;

: decimal ( -- )
    10 base ! ;

: depth ( -- n )
    data_stack 100 cells +  'SP @  - /cell /  2 - ;

: do ( n1 n2 -- ) ( R: -- loop-sys ) ( C: -- do-sys )
    postpone 2>r  here ; immediate

\ TODO: environment?
\ TODO: evaluate
\ TODO: fill
\ TODO: fm/mod )
\ TODO: hold

: j ( -- x1 ) ( R: x1 x2 x3 -- x1 x2 x3 )
    'RP @ 3 cells + @ ;

\ TODO: leave

: loop ( -- ) ( C: nest-sys -- )
    postpone 1  postpone (+loop)
    postpone 0branch  ,
    postpone unloop ; immediate

: lshift   begin ?dup while 1- swap dup + swap repeat ;

: rshift   1 begin over while dup + swap 1- swap repeat nip
           2>r 0 1 begin r@ while
              r> r> 2dup swap dup + 2>r and if swap over + swap then dup +
           repeat r> r> 2drop drop ;

: max ( x y -- max[x,y] )
    2dup > if drop else nip then ;

\ Kernel: min
\ TODO:   mod
\ TODO:   move

: (quit) ( R: ... -- )
    return_stack 100 cells + 'RP !
    0 'source-id !  tib ''source !  #tib ''#source !
    postpone [
    begin
	refill
    while
	interpret  state @ 0= if ." ok" cr then
    repeat
    bye ;

' (quit)  ' quit >body cell+  !

\ TODO: s>d
\ TODO: sign
\ TODO: sm/rem

: spaces ( n -- )
    0 do space loop ;

\ TODO: u.

: signbit ( -- n )   -1 1 rshift invert ;

: xor ( x y -- x^y )    2dup nand >r r@ nand swap r> nand nand ;

: u<  ( x y -- flag )  signbit xor swap signbit xor > ;

\ TODO: um/mod

: variable ( "word" -- )
    create /cell allot ;

: ['] \ ( C: "word" -- )
    ' postpone literal ; immediate
