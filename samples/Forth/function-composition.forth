: compose ( xt1 xt2 -- xt3 )
  >r >r :noname
     r> compile,
     r> compile,
     postpone ;
;

' 2* ' 1+ compose  ( xt )
3 swap execute .   \ 7
