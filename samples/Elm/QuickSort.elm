
main = asText (qsort [3,9,1,8,5,4,7])

qsort lst =
  case lst of
    x:xs -> qsort (filter ((>=)x) xs) ++ [x] ++ qsort (filter ((<)x) xs)
    [] -> []


{---------------------

QuickSort works as follows:
 - Choose a pivot element which be placed in the "middle" of the sorted list.
   In our case we are choosing the first element as the pivot.
 - Gather all of the elements less than the pivot (the first filter).
   We know that these must come before our pivot element in the sorted list.
   Note: ((>=)x) === (\y -> (>=) x y) === (\y -> x >= y)
 - Gather all of the elements greater than the pivot (the second filter).
   We know that these must come after our pivot element in the sorted list.
 - Run `qsort` on the lesser elements, producing a sorted list that contains
   only elements less than the pivot. Put these before the pivot.
 - Run `qsort` on the greater elements, producing a sorted list. Put these
   after the pivot.

Note that choosing a bad pivot can have bad effects. Take a sorted list with
N elements. The pivot will always be the lowest member, meaning that it does
not divide the list very evenly. The list of lessers has 0 elements
and the list of greaters has N-1 elemens. This means qsort will be called
N times, each call looking through the entire list. This means, in the worst
case, QuickSort will make N^2 comparisons.

----------------------}
