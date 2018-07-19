sel=: 1 : 'x # ['

quicksort=: 3 : 0
 if.
  1 >: #y
 do.
  y
 else.
  e=. y{~?#y
  (quicksort y <sel e),(y =sel e),quicksort y >sel e
 end.
)
