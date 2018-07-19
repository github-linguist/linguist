: fsqrt2 1 s>f 0> if 2 s>f else fdup then ;
: fnapier dup dup 1 > if 1- else drop 1 then s>f dup 1 < if drop 2 then s>f ;
: fpi dup 2* 1- dup * s>f 0> if 6 else 3 then s>f ;
                                       ( n -- f1 f2)
: cont.fraction                        ( xt n -- f)
  1 swap 1+ 0 s>f                      \ calculate for 1 .. n
  do i over execute frot f+ f/ -1 +loop
  0 swap execute fnip f+               \ calcucate for 0
;
