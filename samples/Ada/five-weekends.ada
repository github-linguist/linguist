with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar.Formatting;  use Ada.Calendar;

use Ada.Calendar.Formatting;

procedure Five_Weekends is
   Months : Natural := 0;
begin
   for Year in Year_Number range 1901..2100 loop
      for Month in Month_Number range 1..12 loop
         begin
            if Day_Of_Week (Formatting.Time_Of (Year, Month, 31)) = Sunday then
               Put_Line (Year_Number'Image (Year) & Month_Number'Image (Month));
               Months := Months + 1;
            end if;
         exception
            when Time_Error =>
               null;
         end;
      end loop;
   end loop;
   Put_Line ("Number of months:" & Integer'Image (Months));
end Five_Weekends;
