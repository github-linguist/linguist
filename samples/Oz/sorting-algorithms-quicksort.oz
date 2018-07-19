declare
  fun {QuickSort Xs}
     case Xs of nil then nil
     [] Pivot|Xr then
	fun {IsSmaller X} X < Pivot end
        Smaller Larger
     in
	{List.partition Xr IsSmaller ?Smaller ?Larger}
        {Append {QuickSort Smaller} Pivot|{QuickSort Larger}}
     end
  end
in
  {Show {QuickSort [3 1 4 1 5 9 2 6 5]}}
