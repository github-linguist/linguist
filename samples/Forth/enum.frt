\ Implements ENUM.

\ Double DOES>!
: enum   create 0 ,  does> create dup @ 1 rot +! ,  does> @ ;

\ But this is simpler.
: enum   create 0 ,  does> dup @ constant 1 swap +! ;
