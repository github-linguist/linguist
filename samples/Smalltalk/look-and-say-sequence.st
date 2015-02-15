String extend [
  lookAndSay [ |anElement nextElement counter coll newColl|
     coll := (self asOrderedCollection).
     newColl := OrderedCollection new.
     counter := 0.
     anElement := (coll first).
     [ coll size > 0 ]
     whileTrue: [
        nextElement := coll removeFirst.
	( anElement == nextElement ) ifTrue: [
           counter := counter + 1.
        ] ifFalse: [
	  newColl add: (counter displayString).
	  newColl add: (anElement asString).
	  anElement := nextElement.
	  counter := 1.
        ]
     ].
     newColl add: (counter displayString).
     newColl add: (anElement asString).
     ^(newColl join)
  ]
].

|r|
r := '1'.
10 timesRepeat: [
  r displayNl.
  r := r lookAndSay.
]
