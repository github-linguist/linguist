pragma Initialize_Scalars;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Invalid_Value is
   type Color is (Red, Green, Blue);
   X : Float;
   Y : Color;
begin
   if not X'Valid then
      Put_Line ("X is not valid");
   end if;
   X := 1.0;
   if X'Valid then
      Put_Line ("X is" & Float'Image (X));
   end if;
   if not Y'Valid then
      Put_Line ("Y is not valid");
   end if;
   Y := Green;
   if Y'Valid then
      Put_Line ("Y is " & Color'Image (Y));
   end if;
end Invalid_Value;
