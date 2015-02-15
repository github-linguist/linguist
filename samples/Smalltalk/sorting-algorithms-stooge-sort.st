OrderedCollection extend [
    stoogeSortFrom: i to: j [
	(self at: j) < (self at: i)
	  ifTrue: [ self swapElement: i with: j ].
	j - i > 1
          ifTrue: [
	      |t| t := (j - i + 1)//3.
	      self stoogeSortFrom: i to: (j-t).
	      self stoogeSortFrom: (i+t) to: j.
	      self stoogeSortFrom: i to: (j-t)
          ]
    ]
    stoogeSort [ self stoogeSortFrom: 1 to: (self size) ]
    swapElement: i with: j [
	|t| t := self at: i.
        self at: i put: (self at: j).
	self at: j put: t
    ]
].

|test|
test := #( 1 4 5 3 -6 3 7 10 -2 -5) asOrderedCollection.
test stoogeSort.
test printNl.
