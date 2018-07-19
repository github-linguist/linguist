USING: accessors assocs combinators fry grouping hashtables kernel
       locals math math.parser math.ranges random sequences strings
       io ascii ;

IN: bullsncows

TUPLE: score bulls cows ;
: <score> ( -- score ) 0 0 score boa ;

TUPLE: cow ;
: <cow> ( -- cow ) cow new ;

TUPLE: bull ;
: <bull> ( -- bull ) bull new ;

: inc-bulls ( score -- score ) dup bulls>> 1 + >>bulls ;
: inc-cows ( score -- score ) dup cows>> 1 + >>cows ;

: random-nums ( -- seq ) 9 [1,b] 4 sample ;

: add-digits ( seq -- n ) 0 [ swap 10 * + ] reduce number>string ;

: new-number ( -- n narr ) random-nums dup add-digits ;

: narr>nhash ( narr -- nhash ) { 1 2 3 4 } swap zip ;

: num>hash ( n -- hash )
    [ 1string string>number ] { } map-as narr>nhash ;

:: cow-or-bull ( n g -- arr )
    {
        { [ n first g at n second = ] [ <bull> ] }
        { [ n second g value? ] [ <cow> ] }
        [ f ]
    } cond ;

: add-to-score ( arr -- score )
   <score> [ bull? [ inc-bulls ] [ inc-cows ] if ] reduce ;

: check-win ( score -- ? ) bulls>> 4 = ;

: sum-score ( n g -- score ? )
    '[ _ cow-or-bull ] map sift add-to-score dup check-win ;

: print-sum ( score -- str )
    dup bulls>> number>string "Bulls: " swap append swap cows>> number>string
    " Cows: " swap 3append "\n" append ;

: (validate-readln) ( str -- ? ) dup length 4 = not swap [ letter? ] all? or ;

: validate-readln ( -- str )
    readln dup (validate-readln)
    [ "Invalid input.\nPlease enter a valid 4 digit number: "
      write flush drop validate-readln ]
    when ;

: win ( -- ) "\nYou've won! Good job. You're so smart." print flush ;

: main-loop ( x -- )
    "Enter a 4 digit number: " write flush validate-readln num>hash swap
    [ sum-score swap print-sum print flush ] keep swap not
    [ main-loop ] [ drop win ] if ;

: main ( -- ) new-number drop narr>nhash main-loop ;
