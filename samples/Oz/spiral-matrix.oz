declare
  fun {Spiral N}
     %% create nested array
     Arr = {Array.new 1 N unit}
     for Y in 1..N do Arr.Y := {Array.new 1 N 0} end
     %% fill it recursively with increasing numbers
     C = {Counter 0}
  in
     {Fill Arr 1 N C}
     Arr
  end

  proc {Fill Arr S E C}
     %% go right
     for X in S..E do
        Arr.S.X := {C}
     end
     %% go down
     for Y in S+1..E do
        Arr.Y.E := {C}
     end
     %% go left
     for X in E-1..S;~1 do
        Arr.E.X := {C}
     end
     %% go up
     for Y in E-1..S+1;~1 do
        Arr.Y.S := {C}
     end
     %% fill the inner rectangle
     if E - S > 1 then {Fill Arr S+1 E-1 C} end
  end

  fun {Counter N}
     C = {NewCell N}
  in
     fun {$}
        C := @C + 1
     end
  end
in
  {Inspect {Spiral 5}}
