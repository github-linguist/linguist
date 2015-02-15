OrderedCollection extend [
  horner: aValue [
    ^ self reverse inject: 0 into: [:acc :c | acc * aValue + c].
  ]
].

(#(-19 7 -4 6) asOrderedCollection horner: 3) displayNl.
