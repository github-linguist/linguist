| test |

test := [ :a :b :c | a*a+(b*b)=(c*c) ].

(1 to: 20)
    combinations: 3 atATimeDo: [ :x |
        (test valueWithArguments: x)
            ifTrue: [ ':-)' logCr: x ] ].

"output on Transcript:
#(3 4 5)
#(5 12 13)
#(6 8 10)
#(8 15 17)
#(9 12 15)
#(12 16 20)"
