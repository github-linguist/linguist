: th-root { F: a F: n -- a^1/n }
  a
  begin
    a fover n 1e f- f** f/
      fover n 1e f- f*
    f+ n f/
    fswap fover 1e-5 f~
  until ;

34e 5e th-root f.   \ 2.02439745849989
34e 5e 1/f f** f.   \ 2.02439745849989
