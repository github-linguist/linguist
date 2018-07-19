OrderedCollection extend [
  flatten [ |f|
    f := OrderedCollection new.
    self do: [ :i |
      i isNumber
        ifTrue: [ f add: i ]
        ifFalse: [ |t|
          t := (OrderedCollection withAll: i) flatten.
          f addAll: t
        ]
    ].
    ^ f
  ]
].


|list|
list := OrderedCollection
          withAll: { {1} . 2 . { {3 . 4} . 5 } .
                     {{{}}} . {{{6}}} . 7 . 8 . {} }.

(list flatten) printNl.
