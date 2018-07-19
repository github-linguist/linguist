USING: calendar io math.parser threads ;

: read-sleep ( -- )
    readln string>number seconds
    "Sleeping..." print
    sleep
    "Awake!" print ;
