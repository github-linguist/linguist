USING: kernel
    combinators.short-circuit
    math math.combinatorics math.ranges
    sequences
    qw prettyprint ;
IN: rosetta.dinesman

: /= ( x y -- ? ) = not ;
: fifth ( seq -- elt ) 4 swap nth ;

: meets-constraints? ( seq -- ? )
    {
        [ first 5 /= ]                          ! Baker does not live on the top floor.
        [ second 1 /= ]                         ! Cooper does not live on the bottom floor.
        [ third { 1 5 } member? not ]           ! Fletcher does not live on either the top or bottom floor.
        [ [ fourth ] [ second ] bi > ]          ! Miller lives on a higher floor than does Cooper.
        [ [ fifth ] [ third ] bi - abs 1 /= ]   ! Smith does not live on a floor adjacent to Fletcher's.
        [ [ third ] [ second ] bi - abs 1 /= ]  ! Fletcher does not live on a floor adjacent to Cooper's.
    } 1&& ;

: solutions ( -- seq )
    5 [1,b] all-permutations [ meets-constraints? ] filter ;

: >names ( seq -- seq )
    [ 1 - qw{ baker cooper fletcher miller smith } nth ] map ;

: dinesman ( -- )
    solutions [ >names . ] each ;
