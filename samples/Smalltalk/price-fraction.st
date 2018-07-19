"Table driven rescale"
Object subclass: PriceRescale [
  |table|
  PriceRescale class  >> new: theTable [
    ^ self basicNew initialize: theTable
  ]
  initialize: theTable [
     table := theTable asOrderedCollection.
     ^self
  ]
  rescale: aPrice [ |v1 v2|
    1 to: (table size - 1) do: [:i|
      v1 := table at: i.
      v2 := table at: (i+1).
      ((aPrice >= (v1 x)) & (aPrice < (v2 x)))
        ifTrue: [ ^ v1 y ]
    ].
    (aPrice < ((v1:=(table first)) x)) ifTrue: [ ^ v1 y ].
    (aPrice >= ((v1:=(table last)) x)) ifTrue: [ ^ v1 y ]
  ]
].

|pr|
pr := PriceRescale
         new: {  0.00@0.10 .
                 0.06@0.18 .
                 0.11@0.26 .
                 0.16@0.32 .
                 0.21@0.38 .
                 0.26@0.44 .
                 0.31@0.50 .
                 0.36@0.54 .
                 0.41@0.58 .
                 0.46@0.62 .
                 0.51@0.66 .
                 0.56@0.70 .
                 0.61@0.74 .
                 0.66@0.78 .
                 0.71@0.82 .
                 0.76@0.86 .
                 0.81@0.90 .
                 0.86@0.94 .
                 0.91@0.98 .
                 0.96@1.00 .
                 1.01@1.00
               }.

"get a price"
(pr rescale: ( (Random between: 0 and: 100)/100 )) displayNl.
