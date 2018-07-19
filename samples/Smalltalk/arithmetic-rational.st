| sum |
2 to: (2 raisedTo: 19) do: [ :candidate |
  sum := candidate reciprocal.
  2 to: (candidate sqrt) do: [ :factor |
     ( (candidate \\ factor) = 0 )
        ifTrue: [
           sum := sum + (factor reciprocal) + ((candidate / factor) reciprocal)
        ]
  ].
  ( (sum denominator) = 1 )
      ifTrue: [
           ('Sum of recipr. factors of %1 = %2 exactly %3' %
                     { candidate printString .
                       (sum asInteger) printString .
                       ( sum = 1 ) ifTrue: [ 'perfect!' ]
                                   ifFalse: [ ' ' ] }) displayNl
      ]
].
