USING: math.parser splitting ;
: a+b ( -- )
    readln " " split1
    [ string>number ] bi@ +
    number>string print ;
