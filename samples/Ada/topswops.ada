with Ada.Integer_Text_IO, Generic_Perm;

procedure Topswaps is

   function Topswaps(Size: Positive) return Natural is
      package Perms is new Generic_Perm(Size);
      P: Perms.Permutation;
      Done: Boolean;
      Max: Natural;

      function Swapper_Calls(P: Perms.Permutation) return Natural is
	 Q: Perms.Permutation := P;
	 I: Perms.Element := P(1);
      begin
	 if I = 1 then
	    return 0;
	 else
	    for Idx in 1 .. I loop
	       Q(Idx) := P(I-Idx+1);
	    end loop;
	    return 1 + Swapper_Calls(Q);
	 end if;
      end Swapper_Calls;

   begin
      Perms.Set_To_First(P, Done);
      Max:= Swapper_Calls(P);
      while not Done loop
	 Perms.Go_To_Next(P, Done);
	 Max := natural'Max(Max, Swapper_Calls(P));
      end loop;
      return Max;
   end Topswaps;

begin
   for I in 1 .. 10 loop
      Ada.Integer_Text_IO.Put(Item => Topswaps(I), Width => 3);
   end loop;
end Topswaps;
