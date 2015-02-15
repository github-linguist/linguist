declare
  proc {BogoSort Arr}
     for while:{Not {InOrder Arr}} do
        {Shuffle Arr}
     end
  end

  fun {InOrder Arr}
     for I in {Array.low Arr}+1..{Array.high Arr}
	return:Return default:true
     do
        if Arr.(I-1) > Arr.I then {Return false} end
     end
  end

  proc {Shuffle Arr}
     Low = {Array.low Arr}
     High = {Array.high Arr}
  in
     for I in High..Low;~1 do
	J = Low + {OS.rand} mod (I - Low + 1)
        OldI = Arr.I
     in
	Arr.I := Arr.J
        Arr.J := OldI
     end
  end

  X = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {BogoSort X}
  {Show {Array.toRecord unit X}}
