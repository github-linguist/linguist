declare
  fun {IsPerfect N}
     fun {IsNFactor I} N mod I == 0 end
     Factors = {Filter {List.number 1 N-1 1} IsNFactor}
  in
     {Sum Factors} == N
  end

  fun {Sum Xs} {FoldL Xs Number.'+' 0} end
in
  {Show {Filter {List.number 1 10000 1} IsPerfect}}
  {Show {IsPerfect 33550336}}
