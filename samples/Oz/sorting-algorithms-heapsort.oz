declare
  proc {HeapSort A}
     Low = {Array.low A}
     High = {Array.high A}
     Count = High-Low+1

     %% heapify
     LastParent = Low + (Count-2) div 2
  in
     for Start in LastParent..Low;~1 do
        {Siftdown A Start High}
     end

     %% repeatedly put the maximum element to the end
     %% and re-heapify the rest
     for End in High..Low+1;~1 do
        {Swap A End Low}
        {Siftdown A Low End-1}
     end
  end

  proc {Siftdown A Start End}
     Low = {Array.low A}
     fun {FirstChildOf I} Low+(I-Low)*2+1 end

     Root = {NewCell Start}
  in
     for while:{FirstChildOf @Root} =< End
        break:Break
     do
        Child = {NewCell {FirstChildOf @Root}}
     in
        if @Child + 1 =< End andthen A.@Child < A.(@Child + 1) then
           Child := @Child + 1
        end
        if A.@Root < A.@Child then
           {Swap A @Root @Child}
           Root := @Child
        else
           {Break}
        end
     end
  end

  proc {Swap A I J}
     A.J := (A.I := A.J)
  end

  %% create array with indices ~1..7 and fill it
  Arr = {Array.new ~1 7 0}
  {Record.forAllInd unit(~1:3 0:1 4 1 5 9 2 6 5)
   proc {$ I V}
      Arr.I := V
   end}
in
  {HeapSort Arr}
  {Show {Array.toRecord unit Arr}}
