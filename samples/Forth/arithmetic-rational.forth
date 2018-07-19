\ Rationals can use any double cell operations:  2!, 2@, 2dup, 2swap, etc.
\ Uses the stack convention of the built-in "*/" for int * frac -> int

: numerator  drop ;
: denominator nip ;

: s>rat      1 ;		\ integer to rational  (n/1)
: rat>s      / ;		\ integer
: rat>frac   mod ;		\ fractional part
: rat>float  swap s>f s>f f/ ;

: rat.  swap 1 .r [char] / emit . ;

\ normalize: factors out gcd and puts sign into numerator
: gcd ( a b -- gcd ) begin ?dup while tuck mod repeat ;
: rat-normalize ( rat -- rat ) 2dup gcd tuck / >r / r> ;

: rat-abs     swap abs    swap ;
: rat-negate  swap negate swap ;
: 1/rat       over 0< if negate swap negate else swap then ;

: rat+ ( a b c d -- ad+bc bd )
  rot 2dup * >r
   rot * >r * r> +
  r> rat-normalize ;
: rat-  rat-negate rat+ ;

: rat* ( a b c d -- ac bd )
  rot * >r * r> rat-normalize ;
: rat/  swap rat* ;

: rat-equal  d= ;
: rat-less ( a b c d -- ad<bc )
  -rot * >r * r> < ;
: rat-more  2swap rat-less ;

: rat-inc  tuck + swap ;
: rat-dec  tuck - swap ;
