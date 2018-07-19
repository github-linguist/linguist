: sum ( fn start count -- fsum )
  0e
  bounds do
    i s>d d>f dup execute f+
  loop drop ;

:noname ( x -- 1/x^2 ) fdup f* 1/f ;   ( xt )
1 1000 sum f.       \ 1.64393456668156
pi pi f* 6e f/ f.   \ 1.64493406684823
