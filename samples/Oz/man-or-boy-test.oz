declare
  fun {A K X1 X2 X3 X4 X5}
     ReturnA = {NewCell undefined}
     fun {B}
        ReturnB = {NewCell undefined}
     in
        K := @K - 1
        ReturnA := {A {NewCell @K} B X1 X2 X3 X4}
        ReturnB := @ReturnA
        @ReturnB
     end
  in
     if @K =< 0 then ReturnA := {X4} + {X5} else _ = {B} end
     @ReturnA
  end

  fun {C V}
     fun {$} V end
  end
in
  {Show {A {NewCell 10} {C 1} {C ~1} {C ~1} {C 1} {C 0}}}
