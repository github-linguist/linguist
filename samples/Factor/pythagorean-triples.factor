USING: accessors arrays formatting kernel literals math
math.functions math.matrices math.ranges sequences ;
IN: rosettacode.pyth

CONSTANT: T1 {
  {  1  2  2 }
  { -2 -1 -2 }
  {  2  2  3 }
}
CONSTANT: T2 {
  {  1  2  2 }
  {  2  1  2 }
  {  2  2  3 }
}
CONSTANT: T3 {
  { -1 -2 -2 }
  {  2  1  2 }
  {  2  2  3 }
}

CONSTANT: base { 3 4 5 }

TUPLE: triplets-count primitives total ;
: <0-triplets-count> ( -- a ) 0 0 \ triplets-count boa ;
: next-triplet ( triplet T -- triplet' ) [ 1array ] [ m. ] bi* first ;
: candidates-triplets ( seed -- candidates )
  ${ T1 T2 T3 } [ next-triplet ] with map ;
: add-triplets ( current-triples limit triplet -- stop )
  sum 2dup > [
   /i [ + ] curry change-total
   [ 1 + ] change-primitives drop t
  ] [ 3drop f ] if ;
: all-triplets ( current-triples limit seed -- triplets )
  3dup add-triplets [
    candidates-triplets [ all-triplets ] with swapd reduce
  ] [ 2drop ] if ;
: count-triplets ( limit -- count )
  <0-triplets-count> swap base all-triplets ;
: pprint-triplet-count ( limit count -- )
  [ total>> ] [ primitives>> ] bi
  "Up to %d: %d triples, %d primitives.\n" printf ;
: pyth ( -- )
  8 [1,b] [ 10^ dup count-triplets pprint-triplet-count ] each ;
