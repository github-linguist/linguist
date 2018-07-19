⍝  Create a namespace ("hash")
      X←⎕NS ⍬

      ⍝ Assign some names
      X.this←'that'
      X.foo←88

      ⍝  Access the names
      X.this
that

      ⍝  Or do it the array way
      X.(foo this)
88  that

      ⍝  Namespaces are first class objects
      sales ← ⎕NS ⍬
      sales.(prices quantities) ← (100 98.4 103.4 110.16) (10  12 8  10)
      sales.(revenue ← prices +.× quantities)
      sales.revenue
4109.6
