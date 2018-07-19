: agm ( a g -- m )
  begin
    fover fover f+ 2e f/
    frot frot f* fsqrt
    fover fover 1e-15 f~
  until
  fdrop ;

1e  2e -0.5e f**  agm f.   \ 0.847213084793979
