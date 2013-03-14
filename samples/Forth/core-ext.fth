\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

\ Kernel: #tib
\ TODO:   .r

: .( ( "<string><paren>" -- )
    [char] ) parse type ; immediate

: 0<> ( n -- flag )   0 <> ;

: 0> ( n -- flag )   0 > ;

\ Kernel: 2>r

: 2r> ( -- x1 x2 ) ( R: x1 x2 -- )   r> r> r> rot >r swap ;

: 2r@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )   2r> 2dup 2>r ;

: :noname   align here  0 c, 15 allot  lastxt dup @ , !
            [ ' enter >code @ ] literal , 0 , ] lastxt @ ;

\ Kernel: <>

\ : ?do ( n1 n2 -- ) ( R: -- loop-sys ) ( C: -- do-sys )
\     here  postpone 2>r  unresolved branch  here ;

: again ( -- ) ( C: dest -- )
    postpone branch , ; immediate

: string+ ( caddr -- addr )
    count + aligned ;

: (c") ( -- caddr ) ( R: ret1 -- ret2 )
    r> dup string+ >r ;

: c" ( "<string><quote>" -- caddr )
    postpone (c")  [char] " parse  dup c,  string, ; immediate

: case ( -- ) ( C: -- case-sys )
    0 ;

: compile, ( xt -- )
    , ;

\ TODO: convert

: endcase ( x -- ) ( C: case-sys -- )
    0 do  postpone then  loop
    postpone drop ;

: endof ( -- ) ( C: case-sys1 of-sys -- case-sys2 )
    postpone else  swap 1+ ;

\ TODO: erase
\ TODO: expect

: false ( -- 0 )
    0 ;

: hex ( -- )
    16 base ! ;

\ TODO:   marker
\ Kernel: nip

: of ( x x -- | x y -- x ) ( C: -- of-sys )
    postpone over  postpone =  postpone if  postpone drop ;

\ Kernel: pad
\ Kernel: parse

: pick ( xn ... x0 n -- xn ... x0 xn )
    2 + cells 'SP @ + @ ;

: query ( -- )
    tib ''source !  #tib ''#source !  0 'source-id !
    refill drop ;

\ Kernel: refill
\ Kernel: restore-input

\ TODO: roll ( xn xn-1 ... x0 n -- xn-1 ... x0 xn ) ;

\ Kernel: save-input
\ Kernel: source-id
\ TODO:   span
\ Kernel: tib

: to ( x "word" -- )
    ' >body , ;

: true ( -- -1 )
    -1 ;

: tuck ( x y -- y x y )
    swap over ;

\ TODO: u.r

: u> ( x y -- flag )
    2dup u< if 2drop false else <> then ;

\ TODO: unused

: value ( x "word" -- )
    create ,
  does> ( -- x )
    @ ;

: within   over - >r - r> u< ;

\ TODO: [compile]

\ Kernel: \

\ ----------------------------------------------------------------------

( Forth2012 core extension words. )

\ TODO: action-of

\ TODO: buffer:

: defer   create ['] abort ,  does> @ execute ;

: defer! ( xt2 xt1 -- )   >body ! ;

: defer@ ( xt1 -- xt2 )   >body @ ;

\ TODO: holds

: is ( xt "word" -- )   ' defer! ;

\ TODO: parse-name

\ TODO: s\"
