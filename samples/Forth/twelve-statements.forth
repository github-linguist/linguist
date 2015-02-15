: lastbit                              ( n1 -- n2)
  dup if 1 swap begin dup 1 <> while swap 1+ swap 1 rshift repeat drop then
;

: bit 1 swap lshift and 0<> ;          ( n1 n2 -- f)
: bitcount 0 swap begin dup while dup 1- and swap 1+ swap repeat drop ;

12 constant #stat                      \ number of statements
                                       \ encoding of the statements
: s1 >r #stat 12 = r> 0 bit = ;        \ heavy use of binary
: s2 >r r@ 4032 and bitcount 3 = r> 1 bit = ;
: s3 >r r@ 2730 and bitcount 2 = r> 2 bit = ;
: s4 >r r@ 4 bit 0= 96 r@ over and = or r> 3 bit = ;
: s5 >r r@ 14 and 0= r> 4 bit = ;
: s6 >r r@ 1365 and bitcount 4 = r> 5 bit = ;
: s7 >r r@ 1 bit r@ 2 bit xor r> 6 bit = ;
: s8 >r r@ 6 bit 0= 48 r@ over and = or r> 7 bit = ;
: s9 >r r@ 63 and bitcount 3 = r> 8 bit = ;
: s10 >r 3072 r@ over and = r> 9 bit = ;
: s11 >r r@ 448 and bitcount 1 = r> 10 bit = ;
: s12 >r r@ 2047 and bitcount 4 = r> 11 bit = ;
: list #stat 0 do dup i bit if i 1+ . then loop drop ;

: nearmiss?                            \ do we have a near miss?
  over #stat 1- = if                   ( true-pattern #true stat-pattern)
    ." Near miss with statements " dup list ." true (failed "
    >r over invert 1 #stat lshift 1- and lastbit 0 .r ." )" cr r>
  then                                 \ extract the failed statement
;
                                       \ have we found a solution?
: solution?                            ( true-pattern #true stat-pattern)
  over #stat = if ." Solution! with statements " dup list ." true." cr then
;

: 12statements                         \ test the twelve patterns
  1 #stat lshift 0 do                  \ create another bit pattern
    i s12   2* i s11 + 2* i s10 + 2* i s9 + 2* i s8 + 2* i s7 + 2*
    i s6  + 2* i s5  + 2* i s4  + 2* i s3 + 2* i s2 + 2* i s1 +
    abs dup bitcount i solution? nearmiss? drop drop drop
  loop                                 \ count number of bytes and evaluate
;

12statements
