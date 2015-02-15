declare
  fun {Maximum X|Xr}         %% pattern-match on argument to make sure the list is not empty
     {FoldL Xr Value.max X}  %% fold the binary function Value.max over the list
  end
in
  {Show {Maximum [1 2 3 4 3]}}
