with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure Test_Happy_Digits is
   function Is_Happy (N : Positive) return Boolean is
      package Sets_Of_Positive is new Ada.Containers.Ordered_Sets (Positive);
      use Sets_Of_Positive;
      function Next (N : Positive) return Natural is
         Sum   : Natural := 0;
         Accum : Natural := N;
      begin
         while Accum > 0 loop
            Sum   := Sum + (Accum mod 10) ** 2;
            Accum := Accum / 10;
         end loop;
         return Sum;
      end Next;
      Current : Positive := N;
      Visited : Set;
   begin
      loop
         if Current = 1 then
            return True;
         elsif Visited.Contains (Current) then
            return False;
         else
            Visited.Insert (Current);
            Current := Next (Current);
         end if;
      end loop;
   end Is_Happy;
   Found : Natural := 0;
begin
   for N in Positive'Range loop
      if Is_Happy (N) then
         Put (Integer'Image (N));
         Found := Found + 1;
         exit when Found = 8;
      end if;
   end loop;
end Test_Happy_Digits;
