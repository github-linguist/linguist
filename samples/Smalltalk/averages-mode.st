OrderedCollection extend [
  mode [ |s|
     s := self asBag sortedByCount.
     ^ (s select: [ :k | ((s at: 1) key) = (k key) ]) collect: [:k| k value]
  ]
].

#( 1 3 6 6 6 6 7 7 12 12 17 ) asOrderedCollection
    mode displayNl.
#( 1 1 2 4 4) asOrderedCollection
    mode displayNl.
