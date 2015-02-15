: next ( seq -- newseq )
  [ ] [ last ] [ length ] tri
  [ 2 * 1 - 2 * ] [ 1 + ] bi /
  * suffix ;
: Catalan ( n -- seq )  V{ 1 } swap 1 - [ next ] times ;
15 Catalan .
V{
    1
    1
    2
    5
    14
    42
    132
    429
    1430
    4862
    16796
    58786
    208012
    742900
    2674440
}
