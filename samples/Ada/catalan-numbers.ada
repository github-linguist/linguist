with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Catalan is
   function Catalan (N : Natural) return Natural is
      Result : Positive := 1;
   begin
      for I in 1..N loop
         Result := Result * 2 * (2 * I - 1) / (I + 1);
      end loop;
      return Result;
   end Catalan;
begin
   for N in 0..15 loop
      Put_Line (Integer'Image (N) & " =" & Integer'Image (Catalan (N)));
   end loop;
end Test_Catalan;
