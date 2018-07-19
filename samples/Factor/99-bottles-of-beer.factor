USING: io kernel make math math.parser math.ranges sequences ;

: bottle ( -- quot )
    [
        [
            [
                [ # " bottles of beer on the wall,\n" % ]
                [ # " bottles of beer.\n" % ] bi
            ] keep
            "Take one down, pass it around,\n" %
            1 - # " bottles of beer on the wall\n" %
        ] " " make print
    ] ; inline

: last-verse ( -- )
    "Go to the store and buy some more,"
    "no more bottles of beer on the wall!" [ print ] bi@ ;

: bottles ( n -- )
    1 [a,b] bottle each last-verse ;

! Usage: 99 bottles
