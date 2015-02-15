declare
  fun {CreateMatrix Width Height}
    Matrix = {List.make Height}
  in
    for Row in Matrix do
       Row = {List.make Width}
       for X in Row do
          X = {OS.rand} mod 20 +1
       end
    end
    Matrix
  end

  proc {PrintMatrix Matrix}
    %% print until we see 20
     for Row in Matrix break:OuterBreak do
        for X in Row do
           {Show X}
           if X == 20 then {OuterBreak} end
        end
     end
  end
in
  {PrintMatrix {CreateMatrix 10 10}}
