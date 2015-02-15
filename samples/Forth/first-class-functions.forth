: compose ( xt1 xt2 -- xt3 )
  >r >r :noname
     r> compile,
     r> compile,
     postpone ;
;

: cube  fdup fdup f* f* ;
: cuberoot  1e 3e f/ f** ;

: table  create does> swap cells + @ ;

table fn      ' fsin ,  ' fcos ,  ' cube ,
table inverse ' fasin , ' facos , ' cuberoot ,

: main
  3 0 do
    i fn i inverse compose  ( xt )
    0.5e execute f.
  loop ;

main    \ 0.5 0.5 0.5
