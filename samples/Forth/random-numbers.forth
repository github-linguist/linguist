require random.fs
here to seed

-1. 1 rshift 2constant MAX-D	\ or s" MAX-D" ENVIRONMENT? drop

: frnd ( -- f )			\ uniform distribution 0..1
  rnd rnd dabs d>f MAX-D d>f f/ ;

: frnd-normal ( -- f )		\ centered on 0, std dev 1
  frnd pi f* 2e f* fcos
  frnd fln -2e f* fsqrt f* ;

: ,normals ( n -- )		\ store many, centered on 1, std dev 0.5
  0 do frnd-normal 0.5e f* 1e f+ f, loop ;

create rnd-array 1000 ,normals
