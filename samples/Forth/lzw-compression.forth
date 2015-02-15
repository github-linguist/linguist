256 value next-symbol

\ current string fragment

create w 256 allot     \ counted string

: w=c ( c -- )       w 1+ c!        1 w c! ;
: w+c ( c -- )  w count + c!  w c@ 1+ w c! ;

\ Compression

\ dictionary of strings to symbols
0 value dict

: init-dict  table to dict  256 to next-symbol  dict set-current ;

: free-dict                           forth-wordlist set-current ;

: in-dict? ( key len -- ? )		\ can assume len > 1
  dict search-wordlist dup if nip then ;

: lookup-dict ( key len -- symbol )
  dup 1 = if drop c@ exit then
  dict search-wordlist if >body @ else abort" bad-dict!" then ;

: put-dict ( data key len -- )
  nextname create , ;

\ output buffer of symbols
\  in real life, these symbols would be packed into octets
variable out-size
create out 256 cells allot

: output ( symbol -- )
  dup out out-size @ cells + !  1 out-size +!
  dup 256 < if emit space else . then ;

: compress ( addr len -- )
  init-dict  0 out-size !
  over c@ w=c  1 /string
  bounds do
    i c@ w+c
    w count in-dict? 0= if
      w count 1- lookup-dict output
      next-symbol dup w count put-dict
      1+ to next-symbol
      i c@ w=c
    then
  loop
  w count lookup-dict output
  free-dict ;

\ Decompression

\ array of symbols to strings (in real code this would need to be growable)
\  next-symbol is reused for the size of this table
create symtab 256 cells allot
0 value start

: init-symtab  256 to next-symbol  here to start ;

: free-symtab  start here - allot ;

: get-symbol ( symbol -- addr len )
  dup 256 < if pad c! pad 1 exit then
  256 - cells symtab + @ count ;

: add-symbol ( addr len -- )
  here symtab next-symbol 256 - cells + !
  s,
  next-symbol 1+ to next-symbol ;

create entry 256 allot

: decompress ( addr len -- )
  init-symtab
  over @ dup emit w=c
  cells bounds cell+ do
    i @ next-symbol < if
      i @ get-symbol entry place
    else i @ next-symbol = if
      w 1+ c@ w count + c!  w count 1+ entry place
    else
      abort" bad symbol!"
    then then
    entry count type	\ output
    entry 1+ c@ w+c
    w count add-symbol
    entry count w place
  1 cells +loop
  free-symtab ;

\ Testing

s" TOBEORNOTTOBEORTOBEORNOT" compress cr
\ T O B E O R N O T 256 258 260 265 259 261 263

out out-size @ decompress cr
\ TOBEORNOTTOBEORTOBEORNOT
