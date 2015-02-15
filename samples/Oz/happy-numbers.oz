declare
  fun {IsHappy N}
     {IsHappy2 N nil}
  end

  fun {IsHappy2 N Seen}
     if     N == 1          then true
     elseif {Member N Seen} then false
     else
	Next = {Sum {Map {Digits N} Square}}
     in
	{IsHappy2 Next N|Seen}
     end
  end

  fun {Sum Xs}
     {FoldL Xs Number.'+' 0}
  end

  fun {Digits N}
     {Map {Int.toString N} fun {$ D} D - &0 end}
  end

  fun {Square N} N*N end

  fun lazy {Nat I}
     I|{Nat I+1}
  end

  %% List.filter is eager. But we need a lazy Filter:
  fun lazy {LFilter Xs P}
     case Xs of X|Xr andthen {P X} then X|{LFilter Xr P}
     [] _|Xr then {LFilter Xr P}
     [] nil then nil
     end
  end

  HappyNumbers = {LFilter {Nat 1} IsHappy}
in
  {Show {List.take HappyNumbers 8}}
