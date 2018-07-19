| a b |
'a = ' display. a := (stdin nextLine asInteger).
'b = ' display. b := (stdin nextLine asInteger).
( a > b ) ifTrue: [ 'a greater than b' displayNl ].
( a < b ) ifTrue: [ 'a less than b' displayNl ].
( a = b ) ifTrue: [ 'a is equal to b' displayNl ].
