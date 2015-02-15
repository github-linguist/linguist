: pangram? ( str -- ? )
    [ "abcdefghijklmnopqrstuvwxyz" ] dip >lower diff length 0 = ;

"How razorback-jumping frogs can level six piqued gymnasts!" pangram? .
