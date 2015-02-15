|file dict r t|
file := FileStream open: 'unixdict.txt' mode: FileStream read.
dict := Set new.

"load the whole dict into the set before, 'filter' later"
[ file atEnd ] whileFalse: [
  dict add: (file upTo: Character nl) ].

"find those with the sorted letters, and sort them by length"
r := ((dict
       select: [ :w | (w asOrderedCollection sort) = (w asOrderedCollection) ] )
     asSortedCollection: [:a :b| (a size) > (b size) ] ).

"get those that have length = to the max length, and sort alphabetically"
r := (r select: [:w| (w size) = ((r at: 1) size)]) asSortedCollection.

r do: [:e| e displayNl].
