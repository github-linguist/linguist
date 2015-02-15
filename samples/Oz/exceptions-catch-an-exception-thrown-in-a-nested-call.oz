declare
  proc {Foo}
     for I in 1..2 do
        try
           {Bar I}
        catch u0 then {System.showInfo "Procedure Foo caught exception u0"}
        end
     end
  end

  proc {Bar I} {Baz I} end

  proc {Baz I}
     if I == 1 then
        raise u0 end
     else
        raise u1 end
     end
  end
in
  {Foo}
