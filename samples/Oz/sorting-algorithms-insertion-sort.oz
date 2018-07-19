declare
  proc {InsertionSort A}
     Low = {Array.low A}
     High = {Array.high A}
  in
     for I in Low+1..High do
        Value = A.I
        J = {NewCell I-1}
     in
        for while:@J >= Low andthen A.@J > Value do
           A.(@J+1) := A.@J
           J := @J - 1
        end
        A.(@J+1) := Value
     end
  end

  Arr = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {InsertionSort Arr}
  {Show {Array.toRecord unit Arr}}
