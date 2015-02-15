with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
procedure Multiplication_Table is
   package IO is new Integer_IO (Integer);
   use IO;
begin
   Put ("  | ");
   for Row in 1..12 loop
      Put (Row, Width => 4);
   end loop;
   New_Line;
   Put_Line ("--+-" & 12 * 4 * '-');
   for Row in 1..12 loop
      Put (Row, Width => 2);
      Put ("| ");
      for Column in 1..12 loop
         if Column < Row then
            Put ("    ");
         else
            Put (Row * Column, Width => 4);
         end if;
      end loop;
      New_Line;
   end loop;
end Multiplication_Table;
