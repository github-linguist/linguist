declare
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

  X = {Tuple.toArray unit(0 1 2 3 4 5 6 7 8 9)}
in
  {Show {Array.toRecord unit X}}
  {Shuffle X}
  {Show {Array.toRecord unit X}}
