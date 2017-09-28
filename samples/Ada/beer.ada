with Ada.Text_Io; use Ada.Text_Io;
 
procedure Main is
   Y : Integer;
begin
   for X in 1..99 loop
      Y := 100 - X;

      if Y = 1 then
         Put_Line("1 bottle of beer on the wall!");
         Put_Line("1 bottle of beer!");
      else
         Put_Line(Integer'Image(Y) & " bottles of beer on the wall!");
         Put_Line(Integer'Image(Y) & " bottles of beer!");
      end;

      Put_Line("Take one down...");
      Put_Line("Pass it around...");

      if Y = 1 then
         Put_Line("No bottles of beer on the wall!");
      elsif Y = 2 then
         Put_Line("1 bottle of beer on the wall!");
      else
         Put_Line(Integer'Image(Y - 1) & " bottles of beer on the wall!");
      end;

      Put_Line("");
   end loop;
end Main;

