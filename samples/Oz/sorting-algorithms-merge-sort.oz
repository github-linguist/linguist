declare
  fun {MergeSort Xs}
     case Xs
     of nil then nil
     [] [X] then [X]
     else
        Middle = {Length Xs} div 2
        Left Right
        {List.takeDrop Xs Middle ?Left ?Right}
     in
        {List.merge {MergeSort Left} {MergeSort Right} Value.'<'}
     end
  end
in
  {Show {MergeSort [3 1 4 1 5 9 2 6 5]}}
