USING:
    formatting
    fry
    io
    kernel
    math math.parser math.ranges
    prettyprint
    random ;
IN: guessnumber

: game-step ( target -- ? )
    readln string>number
    [
        2dup =
        [ 2drop f "Correct!" ]
        [ < "high" "low" ? "Guess too %s, try again." sprintf t swap ]
        if
    ]
    [ drop t "Invalid guess." ]
    if* print flush ;

: main ( -- )
    99 [1,b]
    [ unparse "Number in range %s, your guess?\n" printf flush ]
    [ random '[ _ game-step ] loop ]
    bi ;
