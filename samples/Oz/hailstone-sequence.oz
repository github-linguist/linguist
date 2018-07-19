declare
  fun {HailstoneSeq N}
     N > 0 = true %% assert
     if N == 1 then         [1]
     elseif {IsEven N} then N|{HailstoneSeq N div 2}
     else                   N|{HailstoneSeq 3*N+1}
     end
  end

  HSeq27 = {HailstoneSeq 27}
  {Length HSeq27} = 112
  {List.take HSeq27 4} = [27 82 41 124]
  {List.drop HSeq27 108} = [8 4 2 1]

  fun {MaxBy2nd A=A1#A2 B=B1#B2}
     if B2 > A2 then B else A end
  end

  Pairs = {Map {List.number 1 99999 1}
           fun {$ I} I#{Length {HailstoneSeq I}} end}

  MaxI#MaxLen = {List.foldL Pairs MaxBy2nd 0#0}
  {System.showInfo
   "Maximum length "#MaxLen#" was found for hailstone("#MaxI#")"}
