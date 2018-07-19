| haystack |
haystack :=
   'Zig,Zag,Wally,Ronald,Bush,Krusty,Charlie,Bush,Bozo' subStrings: $,.
{ 'Washington' . 'Bush'  } do: [:i|
  |t l|
  t := (haystack indexOf: i).
  (t = 0) ifTrue: [ ('%1 is not in the haystack' % { i }) displayNl ]
          ifFalse: [ ('%1 is at index %2' % { i . t }) displayNl.
                     l := ( (haystack size) - (haystack reverse indexOf: i) + 1 ).
                     ( t = l ) ifFalse: [
                       ('last occurence of %1 is at index %2' %
                             { i . l }) displayNl ]
                   ]
].
