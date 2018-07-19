|a b c|
a := OrderedCollection new addAll: #('a' 'b' 'c').
b := OrderedCollection new addAll: #('A' 'B' 'C').
c := OrderedCollection new addAll: #(1 2 3).

1 to: (a size) do: [ :i |
  (a at: i) display.
  (b at: i) display.
  (c at: i) displayNl.
].
