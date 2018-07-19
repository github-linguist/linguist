with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Short_Circuit is
   function A (Value : Boolean) return Boolean is
   begin
      Put (" A=" & Boolean'Image (Value));
      return Value;
   end A;
   function B (Value : Boolean) return Boolean is
   begin
      Put (" B=" & Boolean'Image (Value));
      return Value;
   end B;
begin
   for I in Boolean'Range loop
      for J in Boolean'Range loop
         Put (" (A and then B)=" & Boolean'Image (A (I) and then B (J)));
         New_Line;
      end loop;
   end loop;
   for I in Boolean'Range loop
      for J in Boolean'Range loop
         Put (" (A or else B)=" & Boolean'Image (A (I) or else B (J)));
         New_Line;
      end loop;
   end loop;
end Test_Short_Circuit;
