declare
  proc {SelectionSort Arr}
     proc {Swap K L}
        Arr.K := (Arr.L := Arr.K)
     end
     Low = {Array.low Arr}
     High = {Array.high Arr}
  in
     %% for every index I of the array
     for I in Low..High do
	%% find the index of the minimum element
	%% with an index >= I
	Min = {NewCell Arr.I}
        MinIndex = {NewCell I}
     in
        for J in I..High do
  	 if Arr.J < @Min then
	    Min := Arr.J
	    MinIndex := J
  	 end
	end
	%% and put that minimum element to the left
	{Swap @MinIndex I}
     end
  end

  A = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {SelectionSort A}
  {Show {Array.toRecord unit A}}
