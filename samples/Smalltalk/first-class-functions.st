|forward reverse composer compounds|
"commodities"
Number extend [
   cube [ ^self raisedTo: 3 ]
].
Number extend [
   cubeRoot [ ^self raisedTo: (1 / 3) ]
].

forward := #( #cos #sin #cube ).
reverse := #( #arcCos #arcSin #cubeRoot ).

composer := [ :f :g | [ :x | f value: (g value: x) ] ].

"let us create composed funcs"
compounds := OrderedCollection new.

1 to: 3 do: [ :i |
  compounds add: ([ :j | composer value: [ :x | x perform: (forward at: j) ]
                                  value: [ :x | x perform: (reverse at: j) ] ] value: i)
].

compounds do: [ :r | (r value: 0.5) displayNl ].
