with Ada.Text_IO;  use Ada.Text_IO;

procedure Array_Loop_Test is
   type Array_Index is range 1..3;
   A1 : array (Array_Index) of Character := "abc";
   A2 : array (Array_Index) of Character := "ABC";
   A3 : array (Array_Index) of Integer   := (1, 2, 3);
begin
   for Index in Array_Index'Range loop
      Put_Line (A1 (Index) & A2 (Index) & Integer'Image (A3 (Index))(2));
   end loop;
end Array_Loop_Test;
