\ where unavailable.
: third ( A b c -- A b c A )  >r over r> swap ;
: (.) ( u -- c-addr u )  0 <# #s #> ;

\ COUNT is a standard word with a very different meaning, so this
\ would typically be beheaded, or given another name, or otherwise
\ given a short lifespan, so to speak.
: count ( c-addr1 u1 c -- c-addr1 u1 c+1 u )
  0 2over bounds do
    over i c@ = if 1+ then
  loop swap 1+ swap ;

: self-descriptive? ( u -- f )
  (.) [char] 0 third third bounds ?do
    count i c@ [char] 0 - <> if drop 2drop false unloop exit then
  loop drop 2drop true ;
