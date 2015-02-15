USING: combinators kernel locals math math.ranges sequences sets sorting ;
IN: rosetta.coins

<PRIVATE
! recursive-count uses memoization and local variables.
! coins must be a sequence.
MEMO:: recursive-count ( cents coins -- ways )
    coins length :> types
    {
        ! End condition: 1 way to make 0 cents.
        { [ cents zero? ] [ 1 ] }
        ! End condition: 0 ways to make money without any coins.
        { [ types zero? ] [ 0 ] }
        ! Optimization: At most 1 way to use 1 type of coin.
        { [ types 1 number= ] [
            cents coins first mod zero? [ 1 ] [ 0 ] if
        ] }
        ! Find all ways to use the first type of coin.
        [
            ! f = first type, r = other types of coins.
            coins unclip-slice :> f :> r
            ! Loop for 0, f, 2*f, 3*f, ..., cents.
            0 cents f <range> [
                ! Recursively count how many ways to make remaining cents
                ! with other types of coins.
                cents swap - r recursive-count
            ] [ + ] map-reduce          ! Sum the counts.
        ]
    } cond ;
PRIVATE>

! How many ways can we make the given amount of cents
! with the given set of coins?
: make-change ( cents coins -- ways )
    members [ ] inv-sort-with   ! Sort coins in descending order.
    recursive-count ;
