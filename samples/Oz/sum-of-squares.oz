declare
  fun {SumOfSquares Xs}
     for X in Xs sum:S do
        {S X*X}
     end
  end
in
  {Show {SumOfSquares [3 1 4 1 5 9]}}
