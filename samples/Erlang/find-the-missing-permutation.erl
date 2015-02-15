-module( find_missing_permutation ).

-export( [difference/2, task/0] ).

difference( Permutate_this, Existing_permutations ) -> all_permutations( Permutate_this ) -- Existing_permutations.

task() -> difference( "ABCD", existing_permutations() ).



all_permutations( String ) -> [[A, B, C, D] || A <- String, B <- String, C <- String, D <- String, is_different([A, B, C, D])].

existing_permutations() -> ["ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"].

is_different( [_H] ) -> true;
is_different( [H | T] ) -> not lists:member(H, T) andalso is_different( T ).
