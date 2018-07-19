Y := [:f| [:x| x value: x] value: [:g| f value: [:x| (g value: g) value: x] ] ].

fib := Y value: [:f| [:i| i <= 1 ifTrue: [i] ifFalse: [(f value: i-1) + (f value: i-2)] ] ].

(fib value: 10) displayNl.

fact := Y value: [:f| [:i| i = 0 ifTrue: [1] ifFalse: [(f value: i-1) * i] ] ].

(fact value: 10) displayNl.
