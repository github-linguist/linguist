:: accumulator ( n! -- quot ) [ n + dup n! ] ;

1 accumulator
[ 5 swap call drop ]
[ drop 3 accumulator drop ]
[ 2.3 swap call ] tri .
