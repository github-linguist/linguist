USING: accessors combinators kernel locals math math.order
math.vectors sequences sequences.product combinators.short-circuit ;
IN: knapsack

CONSTANT: values { 3000 1800 2500 }
CONSTANT: weights { 0.3 0.2 2.0 }
CONSTANT: volumes { 0.025 0.015 0.002 }

CONSTANT: max-weight 25.0
CONSTANT: max-volume 0.25

TUPLE: bounty amounts value weight volume ;

: <bounty> ( items -- bounty )
    [ bounty new ] dip {
        [ >>amounts ]
        [ values v. >>value ]
        [ weights v. >>weight ]
        [ volumes v. >>volume ]
    } cleave ;

: valid-bounty? ( bounty -- ? )
    { [ weight>> max-weight <= ]
      [ volume>> max-volume <= ] } 1&& ;

M:: bounty <=> ( a b -- <=> )
    a valid-bounty? [
        b valid-bounty? [
            a b [ value>> ] compare
        ] [ +gt+ ] if
    ] [ b valid-bounty? +lt+ +eq+ ? ] if ;

: find-max-amounts ( -- amounts )
    weights volumes [
        [ max-weight swap / ]
        [ max-volume swap / ] bi* min >integer
    ] 2map ;

: best-bounty ( -- bounty )
    find-max-amounts [ 1 + iota ] map <product-sequence>
    [ <bounty> ] [ max ] map-reduce ;
