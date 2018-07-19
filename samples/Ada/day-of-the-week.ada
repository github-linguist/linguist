with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Text_IO;              use Ada.Text_IO;

procedure Yuletide is
begin
   for Year in 2008..2121 loop
      if Day_Of_Week (Time_Of (Year, 12, 25)) = Sunday then
         Put_Line (Image (Time_Of (Year, 12, 25)));
      end if;
   end loop;
end Yuletide;
