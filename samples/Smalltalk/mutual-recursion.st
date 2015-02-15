|F M ra rb|

F := [ :n |
  (n == 0)
    ifTrue: [ 1 ]
    ifFalse: [ n - (M value: (F value: (n-1))) ]
].

M := [ :n |
  (n == 0)
    ifTrue: [ 0 ]
    ifFalse: [ n - (F value: (M value: (n-1))) ]
].

ra := OrderedCollection new.
rb := OrderedCollection new.
0 to: 19 do: [ :i |
  ra add: (F value: i).
  rb add: (M value: i)
].

ra displayNl.
rb displayNl.
