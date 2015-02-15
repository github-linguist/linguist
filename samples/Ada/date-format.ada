with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Text_IO;              use Ada.Text_IO;

procedure Date_Format is
   function Image (Month : Month_Number) return String is
   begin
      case Month is
         when 1  => return "January";
         when 2  => return "February";
         when 3  => return "March";
         when 4  => return "April";
         when 5  => return "May";
         when 6  => return "June";
         when 7  => return "July";
         when 8  => return "August";
         when 9  => return "September";
         when 10 => return "October";
         when 11 => return "November";
         when 12 => return "December";
      end case;
   end Image;
   function Image (Day : Day_Name) return String is
   begin
      case Day is
         when Monday    => return "Monday";
         when Tuesday   => return "Tuesday";
         when Wednesday => return "Wednesday";
         when Thursday  => return "Thursday";
         when Friday    => return "Friday";
         when Saturday  => return "Saturday";
         when Sunday    => return "Sunday";
      end case;
   end Image;
   Today : Time := Clock;
begin
   Put_Line (Image (Today) (1..10));
   Put_Line
   (  Image (Day_Of_Week (Today)) & ", "
   &  Image (Ada.Calendar.Month (Today))
   &  Day_Number'Image (Ada.Calendar.Day (Today)) & ","
   &  Year_Number'Image (Ada.Calendar.Year (Today))
   );
end Date_Format;
