declare
  fun {Median Xs}
     Len = {Length Xs}
     Mid = Len div 2 + 1 %% 1-based index
     Sorted = {Sort Xs Value.'<'}
  in
     if {IsOdd Len} then {Nth Sorted Mid}
     else ({Nth Sorted Mid} + {Nth Sorted Mid-1}) / 2.0
     end
  end
in
  {Show {Median [4.1 5.6 7.2 1.7 9.3 4.4 3.2]}}
  {Show {Median [4.1 7.2 1.7 9.3 4.4 3.2]}}
