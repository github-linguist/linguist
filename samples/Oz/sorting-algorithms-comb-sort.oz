declare
  proc {CombSort Arr}
     Low = {Array.low Arr}
     High = {Array.high Arr}
     Size = High - Low + 1
     Gap = {NewCell Size}
     Swapped = {NewCell true}
     proc {Swap I J}
        Arr.J := (Arr.I := Arr.J)
     end
  in
     for while:@Gap>1 orelse @Swapped do
        if @Gap > 1 then
           Gap := {Float.toInt {Floor {Int.toFloat @Gap} / 1.3}}
        end
        Swapped := false
        for I in Low..High-@Gap do
           if Arr.I > Arr.(I+@Gap) then
              {Swap I I+@Gap}
              Swapped := true
           end
        end
     end
  end
  Arr = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {CombSort Arr}
  {Show {Array.toRecord unit Arr}}
