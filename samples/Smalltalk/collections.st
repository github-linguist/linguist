|anOrdered aBag aSet aSorted aSorted2 aDictionary|

anOrdered := OrderedCollection new.
anOrdered add: 1; add: 5; add: 3.
anOrdered printNl.

aBag := Bag new.
aBag add: 5; add: 5; add: 5; add: 6.
aBag printNl.

aSet := Set new.
aSet add: 10; add: 5; add: 5; add: 6; add: 10.
aSet printNl.

aSorted := SortedCollection new.
aSorted add: 10; add: 9; add: 8; add: 5.
aSorted printNl.

"another sorted with custom comparator: let's sort
 the other collections according to their size (number of
 elements)"
aSorted2 := SortedCollection sortBlock: [ :a :b |
    (a size) < (b size) ].
aSorted2 add: anOrdered; add: aBag; add: aSet; add: aSorted.
aSorted2 printNl.

aDictionary := Dictionary new.
aDictionary at: 'OrderedCollection' put: anOrdered;
            at: 'Bag' put: aBag;
            at: 'Set' put: aSet;
            at: 'SortedCollection' put: { aSorted. aSorted2 }.
aDictionary printNl.
