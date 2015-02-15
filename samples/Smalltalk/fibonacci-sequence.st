|fibo|
fibo := [ :i |
   |ac t|
   ac := Array new: 2.
   ac at: 1 put: 0 ; at: 2 put: 1.
   ( i < 2 )
     ifTrue: [ ac at: (i+1) ]
     ifFalse: [
        2 to: i do: [ :l |
          t := (ac at: 2).
          ac at: 2 put: ( (ac at: 1) + (ac at: 2) ).
          ac at: 1 put: t
        ].
        ac at: 2.
     ]
].

0 to: 10 do: [ :i |
  (fibo value: i) displayNl
]
