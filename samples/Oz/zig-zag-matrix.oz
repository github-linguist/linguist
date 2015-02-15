declare
  %%            state          move   success     failure
  States = unit(right:        [ 1# 0  downLeft    downInstead]
                downInstead:  [ 0# 1  downLeft    terminate]
                downLeft:     [~1# 1  downLeft    down]
                down:         [ 0# 1  topRight    rightInstead]
                rightInstead: [ 1# 0  topRight    terminate]
                topRight:     [ 1#~1  topRight    right])

  fun {CreateZigZag N}
     ZZ = {Create2DTuple N N}

     %% recursively walk through 2D tuple and set values
     proc {Walk Pos=X#Y Count State}
        [Dir Success Failure] = States.State
        NextPos = {Record.zip Pos Dir Number.'+'}
        Valid = {Record.all NextPos fun {$ C} C > 0 andthen C =< N end}
        NewPos = if Valid then NextPos else Pos end
        NewCount = if Valid then Count + 1 else Count end
        NewState = if Valid then Success else Failure end
     in
        ZZ.Y.X = Count
        if NewState \= terminate then
           {Walk NewPos NewCount NewState}
        end
     end
  in
     {Walk 1#1 0 right}
     ZZ
  end

  fun {Create2DTuple W H}
     T = {MakeTuple unit H}
  in
     {Record.forAll T fun {$} {MakeTuple unit W} end}
     T
  end
in
  {Inspect {CreateZigZag 5}}
