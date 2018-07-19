USING: kernel math locals combinators ;
IN: ackermann

:: ackermann ( m n -- u )
    {
        { [ m 0 = ] [ n 1 + ] }
        { [ n 0 = ] [ m 1 - 1 ackermann ] }
        [ m 1 - m n 1 - ackermann ackermann ]
    } cond ;
