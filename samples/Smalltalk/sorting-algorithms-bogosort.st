Smalltalk at: #isItSorted put: [ :c |
  |isit|
  isit := false.
  (2 to: (c size)) detect: [ :i |
    ( (c at: ( i - 1 )) > (c at: i) )
  ] ifNone: [ isit := true ].
  isit
].
Smalltalk at: #bogosort put: [ :c |
  [ isItSorted value: c ] whileFalse: [
     1 to: (c size) do: [ :i |
        |r t|
        r := (Random between: 1 and: (c size)).
        t := (c at: i).
        c at: i put: (c at: r).
        c at: r put: t
     ]
  ]
].

|tobesorted|
tobesorted := { 2 . 7 . 5 . 3 . 4 . 8 . 6 . 1 }.
bogosort value: tobesorted.
tobesorted displayNl.
