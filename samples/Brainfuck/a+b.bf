INPUT AND SUMMATION
TODO if first symbol is a minus sign print Qgo awayQ
+>                                                  initialize sum to one
++[                                                 loop for each input ie twice
    [>>,----------[----------------------[-<+>]]<]      eat digits until space or newline
    <[<]>>>
    >[<                                                 until no next digit
        ----------------                                    subtract ascii zero minus what we subtracted above
        [->++++++++++<]                                     add ten timess that to the next digit
        <[->+<]<[->+<]>>                                    shift sum and loop counter
        >>
    ]
    <----------------                                   subtract as above from last digit as well
    [-<<+>>]                                            add to sum
    <-
]
<-                                                  subtract original one from sum

OUTPUT
[                                                                                                   while a number divided by ten is bigger than zero
    [->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->--------->+<<[->>>+<<<]]]]]]]]]]>>>[-<<<+>>>]<<<]   divide by ten
    >++++++++++++++++++++++++++++++++++++++++++++++++>                                                  convert remainder to ascii digit
]
<[.<<]                                                                                              print ascii digits
