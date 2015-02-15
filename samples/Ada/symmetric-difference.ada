with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_XOR is
   type Person is (John, Bob, Mary, Serena, Jim);
   type Group is array (Person) of Boolean;
   procedure Put (Set : Group) is
      First : Boolean := True;
   begin
      for I in Set'Range loop
         if Set (I) then
            if First then
               First := False;
            else
               Put (',');
            end if;
            Put (Person'Image (I));
         end if;
      end loop;
   end Put;

   A : Group := (John | Bob | Mary | Serena => True, others => False);
   B : Group := (Jim | Mary | John | Bob    => True, others => False);
begin
   Put ("A xor B = "); Put (A xor B);     New_Line;
   Put ("A - B   = "); Put (A and not B); New_Line;
   Put ("B - A   = "); Put (B and not A); New_Line;
end Test_XOR;
