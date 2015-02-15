Array extend
[
  * anotherArray [
       |acc| acc := 0.
       self with: anotherArray collect: [ :a :b |
          acc := acc + ( a * b )
       ].
       ^acc
  ]
]

( #(1 3 -5) * #(4 -2 -1 ) ) printNl.
