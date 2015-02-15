|ackermann|
ackermann := [ :n :m |
  (n = 0) ifTrue: [ (m + 1) ]
          ifFalse: [
           (m = 0) ifTrue: [ ackermann value: (n-1) value: 1 ]
                   ifFalse: [
                        ackermann value: (n-1)
                                  value: ( ackermann value: n
                                                     value: (m-1) )
                   ]
          ]
].

(ackermann value: 0 value: 0) displayNl.
(ackermann value: 3 value: 4) displayNl.
