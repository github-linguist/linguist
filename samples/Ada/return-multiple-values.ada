with Ada.Text_IO; use Ada.Text_IO;
procedure MultiReturn is
   procedure SumAndDiff (x, y : Integer; sum, diff : out Integer) is begin
      sum := x + y;
      diff := x - y;
   end SumAndDiff;
   inta : Integer := 5;
   intb : Integer := 3;
   thesum, thediff : Integer;
begin
   SumAndDiff (inta, intb, thesum, thediff);
   Put_Line ("Sum:" & Integer'Image (thesum));
   Put_Line ("Diff:" & Integer'Image (thediff));
end MultiReturn;
