#('here' 'are' 'some' 'sample' 'strings' 'to' 'sort' 'since' 'this' 'exercise' 'is' 'not' 'really' 'all' 'that' 'dumb' '(sorry)' ) asSortedCollection
          sortBlock:
                     [:first :second | (second size = first size)
                                            ifFalse: [second size < first size]
                                            ifTrue: [first < second]]
