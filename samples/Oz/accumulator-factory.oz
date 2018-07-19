declare
  fun {Acc Init}
     State = {NewCell Init}
  in
     fun {$ X}
        OldState
     in
        {Exchange State OldState} = {Sum OldState X}
     end
  end

  fun {Sum A B}
     if {All [A B] Int.is} then A+B
     else {ToFloat A}+{ToFloat B}
     end
  end

  fun {ToFloat X}
     if {Float.is X} then X
     elseif {Int.is X} then {Int.toFloat X}
     end
  end

  X = {Acc 1}
in
  {X 5 _}
  {Acc 3 _}
  {Show {X 2.3}}
