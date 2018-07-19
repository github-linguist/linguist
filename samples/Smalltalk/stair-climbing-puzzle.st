Smalltalk at: #stepUp put: 0.
stepUp := [ [ step value ] whileFalse: [ stepUp value ] ].
