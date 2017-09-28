with Ada.Text_Io; use Ada.Text_Io;
 
procedure Main is
begin
   for X in reverse 1..99 loop
      Put_Line(Integer'Image(X) & " bottles of beer on the wall");
      Put_Line(Integer'Image(X) & " bottles of beer");
      Put_Line("Take one down, pass it around");
      Put_Line(Integer'Image(X - 1) & " bottles of beer on the wall");
      New_Line;
   end loop;
end Main;

