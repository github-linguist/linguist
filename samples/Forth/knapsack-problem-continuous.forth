include lib/selcsort.4th               \ use a tiny sorting algorithm

150 value left                         \ capacity in 1/10th kilo

create items                           \ list of items
  ," beef"     38 , 3600 ,             \ description, weight, price (cents)
  ," pork"     54 , 4300 ,             \ weight in 1/10 kilo
  ," ham"      36 , 9000 ,
  ," greaves"  24 , 4500 ,
  ," flitch"   40 , 3000 ,
  ," brawn"    25 , 5600 ,
  ," welt"     37 , 6700 ,
  ," salami"   30 , 9500 ,
  ," sausage"  59 , 9800 ,
  here items - 3 / constant #items     \ total number of items

:redo items swap 3 cells * + ;         \ calculate address of record

#items array (items)                   \ array for sorting
                                       ( a -- n)
: price/weight dup 2 cells + @c swap cell+ @c / ;
: weight@ @ cell+ @c ;                 ( a -- n)
: .item @ @c count type cr ;           ( a --)
                                       \ how to sort: on price/weight
:noname >r price/weight r> price/weight > ; is precedes

: knapsack                             ( --)
  (items) dup #items dup 0 ?do i items (items) i th ! loop sort
  begin                                \ use the sorted array
     dup weight@ left <=               \ still room in the knapsack?
  while
     ." Take all of the " dup .item    \ take all of the item
     left over weight@ - to left cell+ \ adjust knapsack, increment item
  repeat left 100 * dup                \ so how much is left?
                                       \ if room, take as much as possible
  if ." Take " . ." grams of the " .item else drop drop then
;

knapsack
