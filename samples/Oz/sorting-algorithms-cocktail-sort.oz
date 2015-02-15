declare
  proc {CocktailSort Arr}
     proc {Swap I J}
        Arr.J := (Arr.I := Arr.J) %% assignment returns the old value
     end
     IsSorted = {NewCell false}
     Up = {List.number {Array.low Arr} {Array.high Arr}-1 1}
     Down = {Reverse Up}
  in
     for until:@IsSorted break:Break do
	for Range in [Up Down] do
	   IsSorted := true
	   for I in Range do
	      if Arr.I > Arr.(I+1) then
		 IsSorted := false
		 {Swap I I+1}
	      end
	   end
	   if @IsSorted then {Break} end
	end
     end
  end
  Arr = {Tuple.toArray unit(10 9 8 7 6 5 4 3 2 1)}
in
  {CocktailSort Arr}
  {Inspect Arr}
