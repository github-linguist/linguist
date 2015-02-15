declare
  proc {StoogeSort Arr}
     proc {Swap I J}
        Tmp = Arr.I
     in
        Arr.I := Arr.J
        Arr.J := Tmp
     end

     proc {Sort I J}
        Size = J-I+1
     in
        if Arr.J < Arr.I then
           {Swap I J}
        end
        if Size >= 3 then
           Third = Size div 3
        in
           {Sort I J-Third}
           {Sort I+Third J}
           {Sort I J-Third}
        end
     end
  in
     {Sort {Array.low Arr} {Array.high Arr}}
  end

  Arr = {Tuple.toArray unit(1 4 5 3 ~6 3 7 10 ~2 ~5 7 5 9 ~3 7)}
in
  {System.printInfo "\nUnsorted: "}
  for I in {Array.low Arr}..{Array.high Arr} do
     {System.printInfo Arr.I#", "}
  end

  {StoogeSort Arr}

  {System.printInfo "\nSorted  : "}
  for I in {Array.low Arr}..{Array.high Arr} do
     {System.printInfo Arr.I#", "}
  end
