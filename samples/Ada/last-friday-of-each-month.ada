with Ada.Text_IO, GNAT.Calendar.Time_IO, Ada.Command_Line,
  Ada.Calendar.Formatting, Ada.Calendar.Arithmetic;

procedure Last_Weekday_In_Month is

   procedure Put_Line(T: Ada.Calendar.Time) is
      use GNAT.Calendar.Time_IO;
   begin
      Ada.Text_IO.Put_Line(Image(Date => T, Picture => ISO_Date));
   end Put_Line;

   use Ada.Calendar, Ada.Calendar.Arithmetic;
   subtype Day_Name is Formatting.Day_Name; use type Formatting.Day_Name;

   T, Selected : Time;
   Weekday: Day_Name  := Day_Name'Value(Ada.Command_Line.Argument (1));
   Year : Year_Number := Integer'Value (Ada.Command_Line.Argument (2));

begin
   for Month in 1 .. 12 loop
      T := Time_Of (Year => Year, Month => Month, Day => 01);
      while Ada.Calendar.Month(T) = Month loop
	 if Formatting.Day_Of_Week (T) = Weekday then
	    Selected := T;
	 end if;
	 T := T + Day_Count(1);
      end loop;
      Put_Line(Selected);
   end loop;
end Last_Weekday_In_Month;
